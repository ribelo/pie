import fs from "node:fs";
import {conditions, defaults, readResults, quantile, selectConditions} from "./lib.mjs";
process.umask(0o077);
const root = process.argv[2] ?? defaults.corpus + "/experiment";
const results = readResults(root);
const judgments = JSON.parse(fs.readFileSync(root + "/judgments.json", "utf8"));
const key = x => [x.condition, x.id, x.run].join("/");
const labels = new Map(judgments.map(x => [key(x), x]));
if (results.length !== 144 || labels.size !== 144 || judgments.length !== 144) throw new Error("Expected 144 unique judgments and calls");
for (const result of results) {
  const label = labels.get(key(result));
  if (!label || !label.reason || !Number.isInteger(label.jargonErrors) || label.jargonErrors < 0) throw new Error("Missing judgment " + key(result));
  if (result.failure ? label.leak !== null : typeof label.leak !== "boolean") throw new Error("Failure is not a leak judgment " + key(result));
}
function stats(rows) {
  const successful = rows.filter(x => !x.failure);
  const latency = rows.map(x => x.wallMs / 1000);
  const reasoning = rows.map(x => x.usage?.completion_tokens_details?.reasoning_tokens).filter(x => typeof x === "number");
  return {n:rows.length, failures:rows.filter(x => x.failure).length, successful:successful.length,
    leaks:successful.filter(x => labels.get(key(x)).leak).length,
    leaksByRun:[1,2].map(run => successful.filter(x => x.run === run && labels.get(key(x)).leak).length),
    p50Seconds:quantile(latency,.5), p90Seconds:quantile(latency,.9), maxSeconds:latency.length?Math.max(...latency):null,
    medianReasoningTokens:quantile(reasoning,.5), reasoningUsageReported:reasoning.length,
    medianPromptTokens:quantile(rows.map(x => x.usage?.prompt_tokens).filter(x => typeof x === "number"),.5),
    medianCompletionTokens:quantile(rows.map(x => x.usage?.completion_tokens).filter(x => typeof x === "number"),.5),
    costUsd:rows.reduce((sum,x) => sum + (x.usage?.cost ?? 0),0),
    jargonErrors:successful.reduce((sum,x) => sum + labels.get(key(x)).jargonErrors,0),
    outputsWithJargonErrors:successful.filter(x => labels.get(key(x)).jargonErrors > 0).length};
}
const summaries = conditions.map(condition => {
  const rows = results.filter(x => x.condition === condition);
  return {condition, all:stats(rows), short:stats(rows.filter(x => x.durationSeconds < 60))};
});
const selection = selectConditions(summaries);
const {recommendation} = selection;
const scaling = recommendation ? results.filter(x => x.condition === recommendation).reduce((all,x) => {
  let row = all.find(y => y.id === x.id);
  if (!row) { row = {id:x.id,mode:x.mode,durationSeconds:x.durationSeconds,runs:[]}; all.push(row); }
  row.runs.push({run:x.run,failure:x.failure,wallSeconds:x.wallMs/1000,secondsPerAudioMinute:x.wallMs/1000/(x.durationSeconds/60)}); return all;
},[]).map(x => ({...x,runs:x.runs.sort((a,b)=>a.run-b.run),medianSecondsPerAudioMinute:quantile(x.runs.map(y=>y.secondsPerAudioMinute),.5)})).sort((a,b)=>a.durationSeconds-b.durationSeconds) : [];
const summary = {summaries,...selection,scaling,
  startedAt:results.map(x=>x.startedAt).sort()[0],finishedAt:results.map(x=>x.finishedAt).sort().at(-1),
  providers:[...new Set(results.map(x=>x.provider))], models:[...new Set(results.map(x=>x.model))],
  httpStatuses:[...new Set(results.map(x=>x.httpStatus))],finishReasons:[...new Set(results.map(x=>x.finishReason))],
  failures:results.filter(x=>x.failure).map(x=>({condition:x.condition,id:x.id,run:x.run,failure:x.failure})),
  totalCostUsd:summaries.reduce((sum,x)=>sum+x.all.costUsd,0)};
fs.writeFileSync(root + "/summary.json",JSON.stringify(summary,null,2));
// This summary contains metrics and judgments only, never delivered texts.
console.log(JSON.stringify(summary,null,2));
