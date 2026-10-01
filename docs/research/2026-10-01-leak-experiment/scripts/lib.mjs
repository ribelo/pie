import fs from "node:fs";

export const conditions = ["low-raw", "low-schema-text", "low-schema-verbatim", "medium-raw", "medium-schema-text", "medium-schema-verbatim"];
export const defaults = {
  corpus: "/home/ribelo/.local/share/pie/corpus",
  prompts: "/home/ribelo/.config/pie/prompts",
  glossary: "/home/ribelo/.config/pie/glossary.md",
  ffmpeg: "/etc/profiles/per-user/ribelo/bin/ffmpeg",
  ffprobe: "/etc/profiles/per-user/ribelo/bin/ffprobe",
};
export function schema(format) {
  if (format === "raw") return undefined;
  const properties = format === "schema-text" ? {text: {type: "string"}} : {
    verbatim: {type: "string", description: "What the speaker said, written down literally in the order spoken, before any tidying. Working notes belong here."},
    text: {type: "string", description: "The final message to type, exactly as the typist instructions require. Nothing else."},
  };
  return {type: "json_schema", json_schema: {name: "dictation", strict: true, schema: {
    type: "object", properties, required: Object.keys(properties), additionalProperties: false,
  }}};
}
export function requestBody(condition, system, data) {
  const [effort, ...parts] = condition.split("-");
  const format = parts.join("-");
  if (!conditions.includes(condition)) throw new Error("Unknown condition");
  const body = {
    model: "google/gemini-3.8-flash", provider: {order: ["Google AI Studio"]}, temperature: 0.2,
    reasoning: {effort, exclude: true},
    messages: [{role: "system", content: system}, {role: "user", content: [
      {type: "text", text: "Transcribe this recording."},
      {type: "input_audio", input_audio: {data, format: "wav"}},
    ]}],
  };
  const response_format = schema(format);
  if (response_format) body.response_format = response_format;
  return body;
}
export function extract(raw, meta) {
  let response;
  try { response = JSON.parse(raw); } catch { return {failure: "invalid-response-json", delivered: null}; }
  if (!response || typeof response !== "object" || Array.isArray(response)) return {failure: "invalid-response-object", delivered: null};
  const choice = response.choices?.[0];
  const usage = response.usage ?? null;
  const common = {usage, provider: response.provider ?? null, model: response.model ?? null,
    finishReason: choice?.finish_reason ?? null, nativeFinishReason: choice?.native_finish_reason ?? null};
  if (meta.transportError) return {...common, failure: meta.transportError, delivered: null};
  if (meta.httpStatus < 200 || meta.httpStatus >= 300) return {...common, failure: "http-" + meta.httpStatus, delivered: null};
  if (response.error || choice?.error) return {...common, failure: "api-or-choice-error", delivered: null};
  if (choice?.finish_reason !== "stop") return {...common, failure: "finish-" + (choice?.finish_reason ?? "missing"), delivered: null};
  const content = choice?.message?.content;
  if (typeof content !== "string" || !content.trim()) return {...common, failure: "empty-content", delivered: null};
  if (meta.condition.endsWith("-raw")) return {...common, failure: null, delivered: content};
  let value;
  try { value = JSON.parse(content); } catch { return {...common, failure: "non-json-content", delivered: null}; }
  const fields = meta.condition.endsWith("schema-verbatim") ? ["verbatim", "text"] : ["text"];
  if (!value || typeof value !== "object" || Array.isArray(value)
      || Object.keys(value).length !== fields.length || fields.some(field => typeof value[field] !== "string")) {
    return {...common, failure: "schema-violation", delivered: null};
  }
  if (!value.text.trim()) return {...common, failure: "empty-text", delivered: null};
  return {...common, failure: null, delivered: value.text, verbatim: value.verbatim ?? null};
}
export function readResults(root) {
  const plan = JSON.parse(fs.readFileSync(root + "/plan.json", "utf8"));
  return plan.jobs.map(job => {
    const stem = root + "/" + job.condition + "/" + job.id + "." + job.run;
    const meta = JSON.parse(fs.readFileSync(stem + ".meta.json", "utf8"));
    const result = meta.transportError ? {failure: meta.transportError, delivered: null} : extract(fs.readFileSync(stem + ".json", "utf8"), meta);
    return {...job, ...meta, ...result};
  });
}
// The requested decision rule excludes leaks, not failed calls. Report failures separately.
export function selectConditions(summaries) {
  const ordered = [...summaries].sort((a,b) => conditions.indexOf(a.condition)-conditions.indexOf(b.condition));
  const zeroLeaks = ordered.filter(x => x.all.successful > 0 && x.all.leaks === 0);
  const eligible = zeroLeaks.filter(x => x.short.n > 0 && x.short.p50Seconds < 5);
  return {eligible:eligible.map(x=>x.condition), zeroLeaks:zeroLeaks.map(x=>x.condition),
    reliableEligible:eligible.filter(x=>x.all.failures===0).map(x=>x.condition),
    ranking:ordered.map(x=>x.condition), recommendation:eligible[0]?.condition ?? null,
    runnerUp:eligible[1]?.condition ?? null};
}
export function quantile(values, p) {
  if (!values.length) return null;
  const sorted = [...values].sort((a,b) => a-b);
  const index = (sorted.length - 1) * p;
  const low = Math.floor(index), high = Math.ceil(index);
  return sorted[low] + (sorted[high] - sorted[low]) * (index - low);
}
