
import fs from "node:fs";
const orKey = fs.readFileSync("/run/agenix/openrouter-api-key","utf8").trim();
const promptFile = process.argv[2]; const reps = +(process.argv[3]||1);
const glossary = fs.readFileSync("/home/ribelo/.config/pie/glossary.md","utf8");
const prompt = fs.readFileSync(promptFile,"utf8").replaceAll("{{language}}","Polish").replaceAll("{{source_language}}","Polish").replaceAll("{{target_language}}","English").replace("{{glossary}}", glossary);
const tests = JSON.parse(fs.readFileSync("/tmp/piebench/sec/tests.json","utf8"));
const only = process.argv[4] ? process.argv[4].split(",") : null;
const out = [];
await Promise.all(tests.filter(t=>!only||only.includes(t.id)).flatMap(t=>Array.from({length:reps},(_,r)=>(async()=>{
  const b64 = fs.readFileSync("/tmp/piebench/sec/audio/"+t.id+".wav").toString("base64");
  const t0 = performance.now();
  const res = await fetch("https://openrouter.ai/api/v1/responses",{method:"POST",headers:{Authorization:"Bearer "+orKey,"Content-Type":"application/json"},body:JSON.stringify({model:"google/gemini-3.8-flash",temperature:0.2,reasoning:{effort:"low"},provider:{order:["Google AI Studio"]},instructions:prompt,input:[{role:"user",content:[{type:"input_text",text:"The recording follows."},{type:"input_audio",input_audio:{data:b64,format:"wav"}}]}]})});
  const j = await res.json(); const text=(j.output||[]).flatMap(o=>o.content||[]).filter(c=>c.type==="output_text").map(c=>c.text).join("");
  out.push({id:t.id, r, ms:Math.round(performance.now()-t0), text});
})())));
out.sort((a,b)=>a.id.localeCompare(b.id)||a.r-b.r);
for (const o of out) console.log("### "+o.id+" #"+o.r+" ("+o.ms+" ms)\n"+o.text+"\n");
