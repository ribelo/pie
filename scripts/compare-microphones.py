#!/usr/bin/env python3
"""Compare fifine and Anker PowerConf C200 at the current gain.

Does not change PipeWire settings, defaults, or services.
Wizard contract:
  setup OUT
  record OUT silence|speech SECONDS
  analyze OUT
  --self-test
"""

from __future__ import annotations

import argparse
import array
import json
import math
import os
import signal
import subprocess
import sys
import tempfile
import time
import wave
from pathlib import Path

RATE = 48000
FULL_SCALE = 32768.0
CLIP_ABS = 32760
NEAR_SILENCE_DBFS = -45.0
WEAK_CONTRAST_DB = 10.0
TIE_DB = 0.05
LABELS = ("fifine", "anker")
NEEDLES = {"fifine": "fifine", "anker": "anker powerconf c200"}

DISCLAIMERS = (
    "Porównanie dotyczy bieżącego wzmocnienia, nie wewnętrznej czułości mikrofonów.",
    "Sygnał jest mono. Źródło stereo jest miksowane do mono przez PipeWire.",
    "RMS obejmuje mowę, pauzy i tło; nie mierzy odczuwanej głośności LUFS.",
    "Starty nagrań są zbliżone, ale nie zsynchronizowane próbka w próbkę.",
    "To nie jest werdykt jakości odsłuchowej.",
    "Różnica mowa–tło to odejmowanie RMS w dBFS. To nie jest laboratoryjny SNR.",
)


class MicError(Exception):
    pass


class UsageError(MicError):
    pass


def eprint(message: str) -> None:
    print(f"compare-microphones: {message}", file=sys.stderr, flush=True)


def write_json(path: Path, data: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_text(json.dumps(data, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    os.replace(tmp, path)


def run_text(cmd: list[str], timeout: float) -> str:
    try:
        proc = subprocess.run(
            cmd,
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout,
            stdin=subprocess.DEVNULL,
        )
    except subprocess.TimeoutExpired as exc:
        raise MicError(f"przekroczono czas: {cmd[0]}") from exc
    except FileNotFoundError as exc:
        raise MicError(f"brak programu: {cmd[0]}") from exc
    if proc.returncode != 0:
        detail = (proc.stderr or proc.stdout or "").strip()
        raise MicError(f"{cmd[0]} zakończył się kodem {proc.returncode}: {detail}")
    return proc.stdout


def parse_volume(text: str) -> dict:
    value = None
    for line in text.splitlines():
        if line.lower().startswith("volume:"):
            token = line.split(":", 1)[1].strip().split()[0]
            try:
                value = float(token)
            except ValueError:
                value = None
            break
    return {
        "raw": text.strip(),
        "value": value,
        "muted": "MUTED" in text.upper(),
    }


def fmt_volume(volume: dict | None) -> str:
    if not volume:
        return "nieznana"
    if volume.get("value") is None:
        base = volume.get("raw") or "nieznana"
    else:
        base = f"{volume['value']:.2f}"
    return base + (" wyciszony" if volume.get("muted") else "")


def volumes_differ(left: dict | None, right: dict | None) -> bool:
    if not left or not right:
        return left != right
    if bool(left.get("muted")) != bool(right.get("muted")):
        return True
    lv, rv = left.get("value"), right.get("value")
    if lv is None or rv is None:
        return (left.get("raw") or "") != (right.get("raw") or "")
    return abs(float(lv) - float(rv)) > 0.001


def read_volume(node_id: int) -> dict:
    return parse_volume(run_text(["wpctl", "get-volume", str(node_id)], 10))


def props_of(obj: dict) -> dict:
    info = obj.get("info")
    if isinstance(info, dict) and isinstance(info.get("props"), dict):
        return info["props"]
    props = obj.get("props")
    return props if isinstance(props, dict) else {}


def norm_text(*parts: str) -> str:
    return " ".join(parts).casefold().replace("_", " ").replace("-", " ")


def select_sources(dump: object) -> list[dict]:
    if not isinstance(dump, list):
        raise MicError("pw-dump nie jest listą obiektów")
    found = {label: [] for label in LABELS}
    for obj in dump:
        if not isinstance(obj, dict):
            continue
        props = props_of(obj)
        if props.get("media.class") != "Audio/Source":
            continue
        blob = norm_text(str(props.get("node.description") or ""), str(props.get("node.name") or ""))
        for label, needle in NEEDLES.items():
            if needle in blob:
                found[label].append((obj, props))
    selected = []
    for label in LABELS:
        matches = found[label]
        if len(matches) != 1:
            names = [str(props.get("node.name") or "?") for _, props in matches]
            raise MicError(
                f"oczekiwano dokładnie jednego Audio/Source dla {label}, jest {len(matches)}: {', '.join(names) or 'brak'}"
            )
        obj, props = matches[0]
        node_name = props.get("node.name")
        if not node_name:
            raise MicError(f"{label}: brak node.name")
        raw_id = props.get("object.id", obj.get("id"))
        try:
            node_id = int(raw_id)
        except (TypeError, ValueError) as exc:
            raise MicError(f"{label}: brak id węzła") from exc
        serial = props.get("object.serial")
        selected.append(
            {
                "label": label,
                "node_name": str(node_name),
                "node_description": str(props.get("node.description") or node_name),
                "id": node_id,
                "object_serial": int(serial) if str(serial).isdigit() else serial,
                "media_class": "Audio/Source",
            }
        )
    return selected


def load_devices(out: Path) -> dict:
    path = out / "devices.json"
    if not path.is_file():
        raise MicError(f"brak {path}; najpierw uruchom setup")
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise MicError(f"nie można odczytać {path}: {exc}") from exc
    if not isinstance(data, dict) or not isinstance(data.get("devices"), list):
        raise MicError(f"uszkodzony {path}")
    return data


def device_map(doc: dict) -> dict[str, dict]:
    return {item["label"]: item for item in doc["devices"]}


def volume_warnings(doc: dict) -> list[str]:
    warnings = []
    devices = device_map(doc)
    for label, device in devices.items():
        if (device.get("volume") or {}).get("muted"):
            warnings.append(f"{label} jest wyciszony. Nagranie będzie bardzo ciche.")
    phases = doc.get("phases") or {}
    for phase, pdata in phases.items():
        before = pdata.get("volumes_before") or {}
        after = pdata.get("volumes_after") or {}
        for label, volume in before.items():
            setup = (devices.get(label) or {}).get("volume")
            if volumes_differ(setup, volume):
                warnings.append(
                    f"Głośność {label} przy starcie fazy {phase} ({fmt_volume(volume)}) "
                    f"różni się od przygotowania ({fmt_volume(setup)})."
                )
            if label in after and volumes_differ(volume, after[label]):
                warnings.append(
                    f"Głośność {label} zmieniła się w trakcie fazy {phase} "
                    f"({fmt_volume(volume)} → {fmt_volume(after[label])})."
                )
    if "silence" in phases and "speech" in phases:
        for label in devices:
            left = (phases["silence"].get("volumes_before") or {}).get(label)
            right = (phases["speech"].get("volumes_before") or {}).get(label)
            if left and right and volumes_differ(left, right):
                warnings.append(
                    f"Głośność {label} zmieniła się między fazami silence ({fmt_volume(left)}) "
                    f"i speech ({fmt_volume(right)})."
                )
    values = [devices[label].get("volume") for label in LABELS if label in devices]
    if len(values) == 2 and volumes_differ(values[0], values[1]):
        warnings.append(
            "Głośności programowe różnią się już przy przygotowaniu "
            f"(fifine {fmt_volume(values[0])}, anker {fmt_volume(values[1])}). "
            "Różnica poziomu może pochodzić ze wzmocnienia, nie z mikrofonu."
        )
    return warnings


def snapshot_volumes(doc: dict) -> dict[str, dict]:
    snapped = {}
    for device in doc["devices"]:
        snapped[device["label"]] = read_volume(int(device["id"]))
    return snapped


def cmd_setup(out: Path) -> int:
    out.mkdir(parents=True, exist_ok=True)
    dump = json.loads(run_text(["pw-dump"], 20))
    status = run_text(["wpctl", "status"], 15)
    devices = select_sources(dump)
    for device in devices:
        device["volume"] = read_volume(device["id"])
    doc = {
        "version": 1,
        "wpctl_status": status,
        "devices": devices,
        "phases": {},
    }
    write_json(out / "devices.json", doc)
    for device in devices:
        print(
            f"{device['label']}: {device['node_description']} | {device['node_name']} | "
            f"id={device['id']} | głośność={fmt_volume(device['volume'])}",
            flush=True,
        )
    for warning in volume_warnings(doc):
        eprint(warning)
    return 0


def build_record_argv(node_name: str, sample_count: int, wav_path: Path) -> list[str]:
    return [
        "pw-record",
        "--target",
        node_name,
        "--rate",
        str(RATE),
        "--channels",
        "1",
        "--format",
        "s16",
        "--sample-count",
        str(sample_count),
        "-P",
        "node.dont-fallback=true",
        str(wav_path),
    ]


class Owned:
    def __init__(self, term_wait: float = 2.0, kill_wait: float = 2.0) -> None:
        self.term_wait = term_wait
        self.kill_wait = kill_wait
        self.procs: list[subprocess.Popen] = []
        self.logs: list = []

    def spawn(self, cmd: list[str], log_path: Path) -> subprocess.Popen:
        log_path.parent.mkdir(parents=True, exist_ok=True)
        handle = open(log_path, "wb")
        self.logs.append(handle)
        handle.write((" ".join(cmd) + "\n").encode())
        handle.flush()
        proc = subprocess.Popen(
            cmd,
            stdin=subprocess.DEVNULL,
            stdout=handle,
            stderr=subprocess.STDOUT,
            start_new_session=True,
        )
        proc.label = Path(cmd[-1]).name  # type: ignore[attr-defined]
        proc.log_path = log_path  # type: ignore[attr-defined]
        self.procs.append(proc)
        return proc

    def cleanup(self) -> None:
        previous = signal.getsignal(signal.SIGINT)
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        try:
            self._signal_alive(signal.SIGTERM, self.term_wait)
            self._signal_alive(signal.SIGKILL, self.kill_wait)
        finally:
            signal.signal(signal.SIGINT, previous)
            self._close_logs()

    def _signal_alive(self, sig: int, wait_s: float) -> None:
        alive = [proc for proc in self.procs if proc.poll() is None]
        for proc in alive:
            try:
                os.killpg(proc.pid, sig)
            except ProcessLookupError:
                continue
            except PermissionError:
                try:
                    proc.send_signal(sig)
                except ProcessLookupError:
                    continue
        deadline = time.monotonic() + wait_s
        for proc in alive:
            if proc.poll() is not None:
                continue
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            try:
                proc.wait(timeout=remaining)
            except subprocess.TimeoutExpired:
                pass

    def _close_logs(self) -> None:
        while self.logs:
            handle = self.logs.pop()
            try:
                handle.close()
            except OSError:
                pass


def log_tail(path: Path, limit: int = 20) -> str:
    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return ""
    return "\n".join(lines[-limit:])


def run_captures(commands: list[tuple[str, Path, list[str]]], timeout_s: float, owned: Owned | None = None) -> None:
    owned = owned or Owned()
    previous_int = signal.getsignal(signal.SIGINT)
    previous_term = signal.getsignal(signal.SIGTERM)

    def interrupt(signum, frame):  # noqa: ARG001
        raise KeyboardInterrupt

    signal.signal(signal.SIGINT, interrupt)
    signal.signal(signal.SIGTERM, interrupt)
    try:
        for _label, log_path, cmd in commands:
            owned.spawn(cmd, log_path)
        deadline = time.monotonic() + timeout_s
        while True:
            failed = [proc for proc in owned.procs if proc.poll() not in (None, 0)]
            running = [proc for proc in owned.procs if proc.poll() is None]
            if failed:
                details = []
                for proc in failed:
                    details.append(
                        f"{proc.label} kod {proc.returncode}\n{log_tail(proc.log_path)}"
                    )
                raise MicError("nagranie nie powiodło się:\n" + "\n".join(details))
            if not running:
                return
            if time.monotonic() >= deadline:
                names = ", ".join(proc.label for proc in running)
                raise MicError(f"przekroczono limit {timeout_s:.0f} s ({names})")
            time.sleep(min(0.05, max(0.0, deadline - time.monotonic())))
    except BaseException:
        owned.cleanup()
        raise
    finally:
        owned.cleanup()
        signal.signal(signal.SIGINT, previous_int)
        signal.signal(signal.SIGTERM, previous_term)


def read_pcm(path: Path) -> tuple[int, int, int, array.array]:
    try:
        with wave.open(str(path), "rb") as handle:
            channels = handle.getnchannels()
            width = handle.getsampwidth()
            rate = handle.getframerate()
            frames = handle.getnframes()
            raw = handle.readframes(frames)
    except (wave.Error, EOFError, OSError) as exc:
        raise MicError(f"nie można odczytać WAV {path}: {exc}") from exc
    if width != 2:
        raise MicError(f"{path.name}: oczekiwano s16, jest szerokość {width}")
    samples = array.array("h")
    samples.frombytes(raw)
    if sys.byteorder != "little":
        samples.byteswap()
    if channels < 1 or len(samples) != frames * channels:
        raise MicError(f"{path.name}: niepełne dane WAV")
    return channels, rate, frames, samples


def to_mono(samples: array.array, channels: int) -> tuple[array.array, bool]:
    if channels == 1:
        return samples, False
    frames = len(samples) // channels
    mixed = array.array("h")
    for index in range(frames):
        base = index * channels
        total = sum(int(samples[base + channel]) for channel in range(channels))
        value = int(round(total / channels))
        mixed.append(max(-32768, min(32767, value)))
    return mixed, True


def db_fs(amplitude: float) -> float | None:
    if amplitude <= 0:
        return None
    return 20.0 * math.log10(amplitude / FULL_SCALE)


def measure_pcm(samples) -> dict:
    data = samples if isinstance(samples, array.array) else array.array("h", samples)
    count = len(data)
    if count == 0:
        raise MicError("pusty sygnał")
    peak = 0
    clipped = 0
    energy = 0
    for sample in data:
        value = abs(int(sample))
        peak = max(peak, value)
        energy += value * value
        if value >= CLIP_ABS:
            clipped += 1
    rms = math.sqrt(energy / count)
    return {
        "frames": count,
        "rms_dbfs": db_fs(rms),
        "peak_dbfs": db_fs(float(peak)),
        "clipping_fraction": clipped / count,
        "clipping_samples": clipped,
    }


def measure_wav(path: Path) -> dict:
    channels, rate, frames, samples = read_pcm(path)
    mono, downmixed = to_mono(samples, channels)
    if len(mono) == 0:
        raise MicError(f"{path.name}: brak próbek")
    metrics = measure_pcm(mono)
    metrics.update({"rate": rate, "channels": channels, "header_frames": frames, "downmixed": downmixed})
    return metrics


def validate_recording(path: Path, sample_count: int) -> dict:
    metrics = measure_wav(path)
    if metrics["rate"] != RATE:
        raise MicError(f"{path.name}: częstotliwość {metrics['rate']}, oczekiwano {RATE}")
    if metrics["channels"] != 1:
        raise MicError(f"{path.name}: oczekiwano mono, jest {metrics['channels']} kanałów")
    frames = metrics["header_frames"]
    if frames < sample_count:
        raise MicError(f"{path.name}: za krótki ({frames} < {sample_count})")
    if frames > sample_count + RATE // 10:
        raise MicError(f"{path.name}: za długi ({frames} > {sample_count})")
    return metrics


def cmd_record(out: Path, phase: str, seconds: int, runner=run_captures, timeout_s: float | None = None) -> int:
    if phase not in ("silence", "speech"):
        raise UsageError("faza musi być silence albo speech")
    if seconds <= 0 or seconds > 300:
        raise UsageError("sekundy muszą być z zakresu 1..300")
    if not out.is_dir():
        raise MicError(f"brak katalogu {out}; najpierw uruchom setup")
    doc = load_devices(out)
    sample_count = seconds * RATE
    before = snapshot_volumes(doc)
    doc.setdefault("phases", {})[phase] = {
        "seconds": seconds,
        "sample_count": sample_count,
        "volumes_before": before,
    }
    for warning in volume_warnings(doc):
        eprint(warning)
    commands = []
    for device in doc["devices"]:
        wav_path = out / f"{device['label']}-{phase}.wav"
        log_path = out / f"{device['label']}-{phase}.log"
        if wav_path.exists():
            raise MicError(f"{wav_path}: nagranie już istnieje; uruchom nowy test")
        commands.append((device["label"], log_path, build_record_argv(device["node_name"], sample_count, wav_path)))
    limit = seconds + 10 if timeout_s is None else timeout_s
    try:
        runner(commands, limit)
        for device in doc["devices"]:
            validate_recording(out / f"{device['label']}-{phase}.wav", sample_count)
    except BaseException:
        doc["phases"].pop(phase, None)
        write_json(out / "devices.json", doc)
        raise
    try:
        doc["phases"][phase]["volumes_after"] = snapshot_volumes(doc)
    except MicError as exc:
        eprint(f"nie udało się odczytać głośności po nagraniu: {exc}")
    write_json(out / "devices.json", doc)
    for warning in volume_warnings(doc):
        eprint(warning)
    names = ", ".join(f"{device['label']}-{phase}.wav" for device in doc["devices"])
    print(f"nagrano {phase}: {names} ({seconds} s)", flush=True)
    return 0


def fmt_db(value) -> str:
    if value is None:
        return "niedostępny"
    return f"{value:.2f} dBFS"


def fmt_delta(value) -> str:
    if value is None:
        return "niedostępna (to nie jest laboratoryjny SNR)"
    return f"{value:.2f} dB (to nie jest laboratoryjny SNR)"


def level_warnings(report_devices: dict) -> list[str]:
    warnings = []
    for label, device in report_devices.items():
        speech = device["speech"]
        silence = device["silence"]
        rms = speech["rms_dbfs"]
        if rms is None or rms < NEAR_SILENCE_DBFS:
            warnings.append(f"{label}: mowa jest bliska ciszy albo jest cyfrowym zerem.")
        if speech["clipping_fraction"] > 0 or silence["clipping_fraction"] > 0:
            warnings.append(f"{label}: są próbki przy pełnej skali (>= {CLIP_ABS}).")
        delta = device["speech_background_delta_db"]
        if delta is not None and delta < WEAK_CONTRAST_DB:
            warnings.append(
                f"{label}: słaby kontrast mowa–tło ({delta:.1f} dB, próg {WEAK_CONTRAST_DB:.0f} dB). "
                "To nie jest laboratoryjny SNR."
            )
    return warnings


def build_report(doc: dict, measured: dict[str, dict[str, dict]]) -> tuple[dict, str]:
    devices = {}
    for label in LABELS:
        speech = measured[label]["speech"]
        silence = measured[label]["silence"]
        delta = None
        if speech["rms_dbfs"] is not None and silence["rms_dbfs"] is not None:
            delta = speech["rms_dbfs"] - silence["rms_dbfs"]
        source = device_map(doc)[label]
        devices[label] = {
            "node_name": source.get("node_name"),
            "node_description": source.get("node_description"),
            "id": source.get("id"),
            "volume_at_setup": source.get("volume"),
            "silence": silence,
            "speech": speech,
            "speech_background_delta_db": delta,
            "speech_background_delta_is_lab_snr": False,
        }
    fifine = devices["fifine"]["speech"]["rms_dbfs"]
    anker = devices["anker"]["speech"]["rms_dbfs"]
    difference = None if fifine is None or anker is None else fifine - anker
    if difference is None:
        winner = None
        reason = "brak porównywalnego RMS mowy"
    elif abs(difference) < TIE_DB:
        winner = None
        reason = f"różnica mniejsza niż {TIE_DB:.2f} dB"
    else:
        winner = "fifine" if difference > 0 else "anker"
        reason = "wyższy RMS mowy przy bieżącym wzmocnieniu"
    warnings = volume_warnings(doc) + level_warnings(devices)
    report = {
        "version": 1,
        "disclaimers": list(DISCLAIMERS),
        "thresholds": {
            "near_silence_dbfs": NEAR_SILENCE_DBFS,
            "weak_contrast_db": WEAK_CONTRAST_DB,
            "tie_db": TIE_DB,
            "clip_abs": CLIP_ABS,
            "full_scale": FULL_SCALE,
        },
        "devices": devices,
        "speech_rms_difference_db": {"fifine_minus_anker": difference},
        "winner": winner,
        "winner_reason": reason,
        "warnings": warnings,
        "lab_snr": False,
    }
    lines = [
        "Porównanie mikrofonów — bieżące wzmocnienie",
        "===========================================",
        "",
        *DISCLAIMERS,
        "",
        "Urządzenia",
        "----------",
    ]
    for label in LABELS:
        device = devices[label]
        lines.append(
            f"{label}: {device['node_description']} | {device['node_name']} | "
            f"id={device['id']} | głośność={fmt_volume(device['volume_at_setup'])}"
        )
    lines.extend(["", "Pomiary", "-------"])
    for label in LABELS:
        device = devices[label]
        for phase, title in (("silence", "cisza"), ("speech", "mowa")):
            item = device[phase]
            lines.append(
                f"{label}, {title}: RMS {fmt_db(item['rms_dbfs'])}, szczyt {fmt_db(item['peak_dbfs'])}, "
                f"próbek >= {CLIP_ABS}: {item['clipping_fraction']:.6f}"
            )
        lines.append(f"{label}, różnica mowa–tło: {fmt_delta(device['speech_background_delta_db'])}")
    diff_text = "niedostępna" if difference is None else f"{difference:.2f} dB"
    lines.extend(["", f"Różnica RMS mowy (fifine − anker): {diff_text}"])
    if winner is None:
        lines.append("Brak wskazania. RMS mowy nie wyłania głośniejszego mikrofonu.")
    else:
        lines.append(f"Wyższy RMS mowy przy bieżącym wzmocnieniu: {winner}.")
    lines.append("To nie jest werdykt jakości odsłuchowej.")
    lines.extend(["", "Ostrzeżenia", "-----------"])
    lines.extend(f"- {warning}" for warning in warnings)
    if not warnings:
        lines.append("- brak")
    lines.append("")
    return report, "\n".join(lines)


def cmd_analyze(out: Path) -> int:
    doc = load_devices(out)
    measured = {}
    for label in LABELS:
        measured[label] = {}
        for phase in ("silence", "speech"):
            path = out / f"{label}-{phase}.wav"
            if not path.is_file():
                raise MicError(f"brak nagrania {path}")
            measured[label][phase] = measure_wav(path)
    report, text = build_report(doc, measured)
    write_json(out / "report.json", report)
    report_path = out / "report.txt"
    tmp = report_path.with_name(report_path.name + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, report_path)
    print(text, end="", flush=True)
    return 0


def alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def self_test() -> int:
    failures = []

    def check(name: str, condition: bool, detail: str = "") -> None:
        if condition:
            print(f"ok {name}", flush=True)
        else:
            failures.append(f"{name}: {detail}")
            eprint(f"FAIL {name}: {detail}")

    loud = measure_pcm([16384] * 64)
    quiet = measure_pcm([8192] * 64)
    diff = loud["rms_dbfs"] - quiet["rms_dbfs"]
    check("rms-difference-6.0206", round(diff, 4) == 6.0206, f"różnica {diff}")
    same = measure_pcm([4000] * 32)["rms_dbfs"] - measure_pcm([4000] * 32)["rms_dbfs"]
    check("rms-difference-zero", same == 0.0, str(same))
    clipped = measure_pcm([0, 32759, 32760, -32760, 32767, -32768])
    check(
        "clipping",
        clipped["clipping_samples"] == 4 and abs(clipped["clipping_fraction"] - 4 / 6) < 1e-12,
        str(clipped),
    )
    zeros = measure_pcm([0, 0, 0, 0])
    check("silence-zero-null", zeros["rms_dbfs"] is None and zeros["peak_dbfs"] is None, str(zeros))

    def fake_doc(volumes):
        return {
            "devices": [
                {
                    "label": label,
                    "node_name": f"node-{label}",
                    "node_description": label,
                    "id": index + 1,
                    "volume": volumes[label],
                }
                for index, label in enumerate(LABELS)
            ],
            "phases": {},
        }

    def metrics_from(rms, clip=0.0, frames=8):
        return {
            "frames": frames,
            "rate": RATE,
            "channels": 1,
            "downmixed": False,
            "rms_dbfs": rms,
            "peak_dbfs": rms,
            "clipping_fraction": clip,
            "clipping_samples": int(clip * frames),
        }

    winner_doc = fake_doc({"fifine": parse_volume("Volume: 1.50"), "anker": parse_volume("Volume: 0.90")})
    winner_measured = {
        "fifine": {"speech": metrics_from(loud["rms_dbfs"]), "silence": metrics_from(None)},
        "anker": {"speech": metrics_from(quiet["rms_dbfs"]), "silence": metrics_from(None)},
    }
    winner_report, winner_text = build_report(winner_doc, winner_measured)
    check(
        "report-difference",
        round(winner_report["speech_rms_difference_db"]["fifine_minus_anker"], 4) == 6.0206,
        str(winner_report["speech_rms_difference_db"]),
    )
    check("report-null-silence", winner_report["devices"]["fifine"]["silence"]["rms_dbfs"] is None, "")
    check("report-null-delta", winner_report["devices"]["fifine"]["speech_background_delta_db"] is None, "")
    check("report-not-snr", "nie jest laboratoryjny SNR" in winner_text and winner_report["lab_snr"] is False, "")
    check("report-disclaimers", all(item in winner_text for item in DISCLAIMERS), "")
    check("report-winner", winner_report["winner"] == "fifine" and "Wyższy RMS mowy" in winner_text, winner_text)

    tie_measured = {
        label: {"speech": metrics_from(-20.0), "silence": metrics_from(-40.0)} for label in LABELS
    }
    tie_report, tie_text = build_report(fake_doc({"fifine": parse_volume("Volume: 1.00"), "anker": parse_volume("Volume: 1.00")}), tie_measured)
    check("report-no-winner-zero", tie_report["winner"] is None and tie_report["speech_rms_difference_db"]["fifine_minus_anker"] == 0.0, str(tie_report["winner"]))
    check("report-no-winner-text", "Brak wskazania" in tie_text and "Wyższy RMS mowy przy bieżącym wzmocnieniu:" not in tie_text, tie_text)

    weak = {
        "fifine": {"speech": metrics_from(-50.0, clip=0.5), "silence": metrics_from(-55.0)},
        "anker": {"speech": metrics_from(-12.0), "silence": metrics_from(-14.0)},
    }
    weak_report, weak_text = build_report(fake_doc({"fifine": parse_volume("Volume: 1.00"), "anker": parse_volume("Volume: 1.00")}), weak)
    joined = "\n".join(weak_report["warnings"])
    check("report-warnings", "bliska ciszy" in joined and "pełnej skali" in joined and "słaby kontrast" in joined, joined)
    check("report-warning-text", "Ostrzeżenia" in weak_text, "")

    changed = fake_doc({"fifine": parse_volume("Volume: 1.50"), "anker": parse_volume("Volume: 0.90")})
    changed["phases"] = {
        "silence": {"volumes_before": {"fifine": parse_volume("Volume: 1.50"), "anker": parse_volume("Volume: 0.90")}},
        "speech": {"volumes_before": {"fifine": parse_volume("Volume: 1.20"), "anker": parse_volume("Volume: 0.90")}},
    }
    changed_text = "\n".join(volume_warnings(changed))
    check("volume-changed-across-phases", "zmieniła się między fazami" in changed_text and "fifine" in changed_text, changed_text)

    dump = [
        {"id": 1, "info": {"props": {"media.class": "Audio/Sink", "node.description": "fifine Microphone Analog Stereo", "node.name": "sink-fifine"}}},
        {"id": 2, "info": {"props": {"media.class": "Video/Source", "node.description": "Anker PowerConf C200 (V4L2)", "node.name": "v4l2-anker"}}},
        {"id": 50, "info": {"props": {"media.class": "Audio/Source", "node.description": "fifine Microphone Mono", "node.name": "alsa_input.fifine", "object.id": 50, "object.serial": 65}}},
        {"id": 47, "info": {"props": {"media.class": "Audio/Source", "node.description": "Anker PowerConf C200 Analog Stereo", "node.name": "alsa_input.Anker_PowerConf_C200", "object.id": 47, "object.serial": 68}}},
    ]
    selected = select_sources(dump)
    check(
        "detect-sources",
        [item["label"] for item in selected] == ["fifine", "anker"] and selected[0]["id"] == 50 and selected[1]["node_name"].startswith("alsa_input.Anker"),
        str(selected),
    )
    dup = json.loads(json.dumps(dump))
    dup.append(dup[-2])
    try:
        select_sources(dup)
        check("detect-duplicate", False, "brak błędu")
    except MicError as exc:
        check("detect-duplicate", "fifine" in str(exc), str(exc))

    argv = build_record_argv("alsa_input.example", 5 * RATE, Path("fifine-silence.wav"))
    check(
        "record-argv",
        argv == [
            "pw-record", "--target", "alsa_input.example", "--rate", "48000", "--channels", "1",
            "--format", "s16", "--sample-count", str(5 * RATE), "-P", "node.dont-fallback=true",
            "fifine-silence.wav",
        ],
        str(argv),
    )

    with tempfile.TemporaryDirectory() as raw:
        root = Path(raw)
        pid_path = root / "sleeper.pid"
        sleeper = (
            "import os,signal,sys,time,pathlib;"
            "pathlib.Path(sys.argv[1]).write_text(str(os.getpid()));"
            "signal.signal(signal.SIGTERM, signal.SIG_IGN);"
            "time.sleep(60)"
        )
        failer = "import sys,time; time.sleep(0.4); sys.exit(3)"
        commands = [
            ("fifine", root / "fifine-silence.log", [sys.executable, "-c", sleeper, str(pid_path)]),
            ("anker", root / "anker-silence.log", [sys.executable, "-c", failer]),
        ]
        try:
            run_captures(commands, 5, Owned(term_wait=0.2, kill_wait=1.0))
            check("cleanup-failure", False, "brak błędu")
        except MicError:
            pid = int(pid_path.read_text()) if pid_path.exists() else 0
            check("cleanup-failure", pid > 0 and not alive(pid), f"pid={pid}")
        pid_path.unlink(missing_ok=True)
        commands = [
            ("fifine", root / "fifine-speech.log", [sys.executable, "-c", sleeper, str(pid_path)]),
            ("anker", root / "anker-speech.log", [sys.executable, "-c", sleeper, str(root / "other.pid")]),
        ]
        try:
            run_captures(commands, 0.3, Owned(term_wait=0.2, kill_wait=1.0))
            check("cleanup-timeout", False, "brak błędu")
        except MicError as exc:
            pid = int(pid_path.read_text()) if pid_path.exists() else 0
            check("cleanup-timeout", "przekroczono limit" in str(exc) and pid > 0 and not alive(pid), f"{exc} pid={pid}")

    if failures:
        eprint(f"self-test: {len(failures)} niepowodzeń")
        return 1
    print("self-test: ok", flush=True)
    return 0


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="compare-microphones.py",
        description="Porównuje fifine i Anker PowerConf C200 przy bieżącym wzmocnieniu. Nie zmienia ustawień.",
    )
    parser.add_argument("--self-test", action="store_true", help="testy syntetyczne, bez nagrywania")
    parser.add_argument("--version", action="version", version="compare-microphones.py 1")
    sub = parser.add_subparsers(dest="cmd")
    setup = sub.add_parser("setup", help="wykryj źródła i zapisz devices.json")
    setup.add_argument("out")
    record = sub.add_parser("record", help="nagraj oba źródła jednocześnie")
    record.add_argument("out")
    record.add_argument("phase", choices=("silence", "speech"))
    record.add_argument("seconds", type=int)
    analyze = sub.add_parser("analyze", help="policz poziomy i zapisz raport")
    analyze.add_argument("out")
    if not argv:
        parser.print_help()
        raise SystemExit(0)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    try:
        if args.self_test:
            return self_test()
        out = Path(args.out)
        if args.cmd == "setup":
            return cmd_setup(out)
        if args.cmd == "record":
            return cmd_record(out, args.phase, args.seconds)
        if args.cmd == "analyze":
            return cmd_analyze(out)
        eprint("podaj setup, record, analyze albo --self-test")
        return 2
    except UsageError as exc:
        eprint(str(exc))
        return 2
    except MicError as exc:
        eprint(str(exc))
        return 1
    except KeyboardInterrupt:
        eprint("przerwano, zatrzymuję nagrania")
        return 130
    except Exception as exc:
        eprint(f"nieoczekiwany błąd: {exc}")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
