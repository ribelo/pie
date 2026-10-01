// Prepare private, complete texts for manual reading; this does not judge leaks.
import fs from "node:fs";
import path from "node:path";
import {conditions, defaults, extract} from "./lib.mjs";
process.umask(0o077);
const root = path.resolve(process.argv[2] ?? defaults.corpus + "/experiment");
const plan = JSON.parse(fs.readFileSync(root + "/plan.json", "utf8"));
const ids = process.argv[3] ? [process.argv[3]] : plan.recordings.map(x => x.id);
const wrap = text => text.split("\n").flatMap(line => {
  const words = line.split(/\s+/); const lines = []; let current = "";
  for (const word of words) { if (current.length + word.length > 120) {lines.push(current); current = "";} current += (current ? " " : "") + word; }
  lines.push(current); return lines;
}).join("\n");
fs.mkdirSync(root + "/review", {recursive: true, mode: 0o700});
for (const id of ids) {
  const item = plan.recordings.find(x => x.id === id);
  if (!item) throw new Error("Unknown recording");
  let text = "# " + id + " | " + item.mode + " | " + item.durationSeconds + " s\n";
  let ready = 0;
  for (const condition of conditions) for (const run of [1,2]) {
    const stem = root + "/" + condition + "/" + id + "." + run;
    text += "\n## " + condition + " run " + run + "\n";
    if (!fs.existsSync(stem + ".meta.json")) {text += "PENDING\n"; continue;}
    const meta = JSON.parse(fs.readFileSync(stem + ".meta.json", "utf8"));
    const result = meta.transportError ? {failure: meta.transportError} : extract(fs.readFileSync(stem + ".json", "utf8"), meta);
    ready++;
    text += "Latency " + (meta.wallMs / 1000).toFixed(3) + " s; reasoning tokens " + (result.usage?.completion_tokens_details?.reasoning_tokens ?? "missing") + "\n\n";
    text += result.failure ? "FAILURE: " + result.failure + "\n" : wrap(result.delivered) + "\n";
  }
  fs.writeFileSync(root + "/review/" + id + ".md", text);
  console.log(JSON.stringify({id, ready, expected:12, review:root + "/review/" + id + ".md"}));
}
