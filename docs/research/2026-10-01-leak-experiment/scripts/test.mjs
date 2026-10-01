import assert from "node:assert/strict";
import {conditions, requestBody, extract, quantile, selectConditions} from "./lib.mjs";
const response = content => JSON.stringify({choices: [{finish_reason: "stop", message: {content}}], usage: {completion_tokens_details: {reasoning_tokens: 0}}});
const meta = condition => ({condition, httpStatus: 200});
assert.equal(conditions.length, 6);
assert.equal(requestBody("low-raw", "prompt", "audio").response_format, undefined);
const body = requestBody("medium-schema-verbatim", "prompt", "audio");
assert.deepEqual(body.reasoning, {effort: "medium", exclude: true});
assert.deepEqual(body.provider.order, ["Google AI Studio"]);
assert.equal(body.messages[1].content[1].input_audio.format, "wav");
assert.deepEqual(body.response_format.json_schema.schema.required, ["verbatim", "text"]);
assert.deepEqual(Object.keys(body.response_format.json_schema.schema.properties), ["verbatim", "text"]);
assert.equal(extract(response("final"), meta("low-raw")).delivered, "final");
assert.equal(extract(response('{"text":"final"}'), meta("low-schema-text")).delivered, "final");
assert.equal(extract(response('{"verbatim":"working notes","text":"final"}'), meta("low-schema-verbatim")).delivered, "final");
for (const content of ["not JSON", '{"text":"final","extra":true}', '{"text":4}', '{"verbatim":"notes"}', '[]', 'null']) {
  assert.ok(extract(response(content), meta("low-schema-text")).failure);
}
assert.ok(extract(response('{"text":"final"}'), meta("low-schema-verbatim")).failure);
assert.ok(extract(JSON.stringify({choices:[{finish_reason:"error",error:{code:500}}]}),meta("low-raw")).failure);
assert.ok(extract(response("final"), {condition:"low-raw",httpStatus:429}).failure);
assert.ok(extract("bad",meta("low-raw")).failure);
assert.ok(extract("null",meta("low-raw")).failure);
assert.ok(extract("[]",meta("low-raw")).failure);
assert.equal(quantile([1,2,3,4],0.5),2.5);
assert.equal(quantile([],0.5),null);
const selection = selectConditions([
  {condition:"medium-raw",all:{successful:24,leaks:0,failures:0},short:{n:14,p50Seconds:6.2}},
  {condition:"low-raw",all:{successful:22,leaks:3,failures:2},short:{n:14,p50Seconds:2.1}},
  {condition:"low-schema-verbatim",all:{successful:20,leaks:0,failures:4},short:{n:14,p50Seconds:3.6}},
  {condition:"low-schema-text",all:{successful:20,leaks:0,failures:4},short:{n:14,p50Seconds:2.2}},
]);
assert.equal(selection.recommendation,"low-schema-text");
assert.equal(selection.runnerUp,"low-schema-verbatim");
assert.deepEqual(selection.reliableEligible,[]);
assert.equal(selectConditions([{condition:"low-raw",all:{successful:0,leaks:0,failures:24},short:{n:14,p50Seconds:2}}]).recommendation,null);
console.log("All request, schema, extraction, quantile and selection checks passed.");
