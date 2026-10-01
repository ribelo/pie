// Paid API calls. Audio, prompts, responses and transcripts stay outside the repository.
import fs from "node:fs";
import path from "node:path";
import crypto from "node:crypto";
import {execFileSync} from "node:child_process";
import {conditions, defaults, requestBody, extract} from "./lib.mjs";

process.umask(0o077);
const corpus = process.argv[2] ?? defaults.corpus;
const root = path.resolve(process.argv[3] ?? path.join(corpus, "experiment"));
const concurrency = Number(process.argv[4] ?? 4);
const prepareOnly = process.argv.includes("--prepare-only");
if (![3, 4].includes(concurrency)) throw new Error("Concurrency must be 3 or 4");
const repo = execFileSync("git", ["rev-parse", "--show-toplevel"], {encoding: "utf8"}).trim();
if (root === repo || root.startsWith(repo + path.sep)) throw new Error("Private output must be outside the repository");
const hash = value => crypto.createHash("sha256").update(value).digest("hex");
const manifest = JSON.parse(fs.readFileSync(path.join(corpus, "manifest.json"), "utf8"));
if (manifest.length !== 12 || new Set(manifest.map(x => x.id)).size !== 12) throw new Error("Expected exactly 12 distinct recordings");
fs.mkdirSync(path.join(root, "wav"), {recursive: true, mode: 0o700});
const glossary = fs.readFileSync(defaults.glossary, "utf8");
const systems = Object.fromEntries(["transcribe", "translate"].map(mode => {
  const name = mode === "translate" ? "one-pass-translate.md" : "one-pass.md";
  return [mode, fs.readFileSync(path.join(defaults.prompts, name), "utf8")
    .replaceAll("{{language}}", "Polish").replaceAll("{{source_language}}", "Polish")
    .replaceAll("{{target_language}}", "English").replaceAll("{{glossary}}", glossary)];
}));
const recordings = manifest.map(item => {
  if (!["transcribe", "translate"].includes(item.mode) || !/^[0-9-]+$/.test(item.id)) throw new Error("Invalid corpus item " + item.id);
  const input = path.join(corpus, item.id + ".opus");
  const wav = path.join(root, "wav", item.id + ".wav");
  const sourceSha256 = hash(fs.readFileSync(input));
  execFileSync(defaults.ffmpeg, ["-nostdin", "-hide_banner", "-loglevel", "error", "-y", "-i", input, "-vn", "-ar", "16000", "-ac", "1", "-c:a", "pcm_s16le", wav]);
  const probe = JSON.parse(execFileSync(defaults.ffprobe, ["-v", "error", "-show_entries", "format=duration:stream=codec_name,sample_rate,channels", "-of", "json", wav], {encoding: "utf8"}));
  const stream = probe.streams[0];
  if (stream.codec_name !== "pcm_s16le" || stream.sample_rate !== "16000" || stream.channels !== 1) throw new Error("Unexpected WAV encoding");
  return {id: item.id, mode: item.mode, historicallyLeaked: item.leaked, durationSeconds: Number(probe.format.duration), sourceSha256, wavSha256: hash(fs.readFileSync(wav)), wav};
});
const jobs = recordings.flatMap(recording => conditions.flatMap(condition => [1,2].map(run => ({
  id: recording.id, mode: recording.mode, durationSeconds: recording.durationSeconds, condition, run,
}))));
// Fixed Fisher-Yates shuffle: conditions and repeats are interleaved rather than batched.
let state = 20261001;
const random = () => { state ^= state << 13; state ^= state >>> 17; state ^= state << 5; return (state >>> 0) / 4294967296; };
for (let i = jobs.length - 1; i > 0; i--) { const j = Math.floor(random() * (i+1)); [jobs[i], jobs[j]] = [jobs[j], jobs[i]]; }
const plan = {version: 1, seed: 20261001, concurrency, timeoutMs: 180000,
  model: "google/gemini-3.8-flash", providerOrder: ["Google AI Studio"], temperature: 0.2,
  glossarySha256: hash(glossary), promptSha256: Object.fromEntries(Object.entries(systems).map(([k,v]) => [k, hash(v)])), recordings, jobs};
const planPath = path.join(root, "plan.json");
if (fs.existsSync(planPath) && JSON.stringify(JSON.parse(fs.readFileSync(planPath, "utf8"))) !== JSON.stringify(plan)) throw new Error("Existing experiment has a different plan; choose a new output directory");
fs.writeFileSync(planPath, JSON.stringify(plan, null, 2));
console.log(JSON.stringify({prepared: jobs.length, root, shortRecordings: recordings.filter(x => x.durationSeconds < 60).length}));
if (prepareOnly) process.exit(0);
const key = fs.readFileSync("/run/agenix/openrouter-api-key", "utf8").trim();
const audio = Object.fromEntries(recordings.map(item => [item.id, fs.readFileSync(item.wav).toString("base64")]));
let cursor = 0, completed = 0;
await Promise.all(Array.from({length: concurrency}, async () => {
  while (cursor < jobs.length) {
    const job = jobs[cursor++];
    const directory = path.join(root, job.condition);
    fs.mkdirSync(directory, {recursive: true, mode: 0o700});
    const stem = path.join(directory, job.id + "." + job.run);
    if (fs.existsSync(stem + ".meta.json")) { completed++; continue; }
    if (fs.existsSync(stem + ".json")) throw new Error("Orphan raw response: inspect before rerunning " + stem);
    const payload = JSON.stringify(requestBody(job.condition, systems[job.mode], audio[job.id]));
    const meta = {...job, startedAt: new Date().toISOString(), requestSha256: hash(payload), httpStatus: null, transportError: null};
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 180000);
    const t0 = performance.now();
    let raw = "";
    try {
      const res = await fetch("https://openrouter.ai/api/v1/chat/completions", {
        method: "POST", headers: {Authorization: "Bearer " + key, "Content-Type": "application/json"}, body: payload, signal: controller.signal,
      });
      meta.httpStatus = res.status;
      raw = await res.text();
    } catch (error) {
      meta.transportError = controller.signal.aborted ? "timeout" : "transport-" + error.name;
    } finally {
      meta.wallMs = performance.now() - t0;
      meta.finishedAt = new Date().toISOString();
      clearTimeout(timer);
    }
    fs.writeFileSync(stem + ".json", raw);
    fs.writeFileSync(stem + ".meta.json", JSON.stringify(meta, null, 2));
    const result = meta.transportError ? {failure: meta.transportError} : extract(raw, meta);
    console.log(JSON.stringify({completed: ++completed, ...job, wallSeconds: +(meta.wallMs/1000).toFixed(3), httpStatus: meta.httpStatus,
      finishReason: result.finishReason ?? null, failure: result.failure, reasoningTokens: result.usage?.completion_tokens_details?.reasoning_tokens ?? null}));
  }
}));
console.log(JSON.stringify({complete: completed, expected: 144}));
