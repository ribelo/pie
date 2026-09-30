
import fs from "node:fs";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
const run = promisify(execFile);
const orKey = fs.readFileSync("/run/agenix/openrouter-api-key","utf8").trim();
const gKey = process.env.GEMINI_API_KEY;
const {twoPassSys, onePassSys} = JSON.parse(fs.readFileSync("/tmp/piebench/cmp-prompts.json","utf8"));
const samples = ["s1","s2","s3","s4","s5","s6","s7","s8","s9","s10"];
const REPS = +(process.argv[2]||3);
const out = {pass1:[], pass2:[], one:[], google:[]};
const pool = async (jobs, n, fn) => { let k=0; await Promise.all(Array.from({length:n}, async()=>{ while(k<jobs.length){ const j=jobs[k++]; await fn(j);} })); };
async function sse(url, headers, body){
  const t0=performance.now(); let ttft=null, text="", usage=null;
  const res = await fetch(url,{method:"POST",headers:{"Content-Type":"application/json",...headers},body:JSON.stringify(body),signal:AbortSignal.timeout(60000)});
  if(!res.ok) return {err:res.status+" "+(await res.text()).slice(0,300), ms:performance.now()-t0};
  const dec=new TextDecoder(); let buf="";
  for await (const ch of res.body){ buf+=dec.decode(ch,{stream:true}); let i; while((i=buf.indexOf("\n"))>=0){ const line=buf.slice(0,i).trim(); buf=buf.slice(i+1); if(!line.startsWith("data:")) continue; const d=line.slice(5).trim(); if(d==="[DONE]") continue; try{ const j=JSON.parse(d); if(j.usage) usage=j.usage;
      const c = j.choices?.[0]?.delta?.content ?? (j.candidates?.[0]?.content?.parts||[]).filter(p=>!p.thought).map(p=>p.text||"").join("");
      if(c){ if(ttft===null) ttft=performance.now()-t0; text+=c; } if(j.usageMetadata) usage=j.usageMetadata; }catch{} } }
  return {ms:performance.now()-t0, ttft, text:text.trim(), usage};
}
// Pass 1 (streaming gpt-live-transcribe via Codex, real-time feed)
for (let r=0;r<REPS;r++){
  await Promise.all(samples.map(async s=>{ try{ const {stdout}=await run("node",["/tmp/piebench/rt.mjs","gpt-live-transcribe",s,"realtime"],{maxBuffer:1e7}); const j=JSON.parse(stdout.trim().split("\n").pop()); out.pass1.push({s,r,ms:j.afterCommitMs,text:j.text}); }catch(e){ out.pass1.push({s,r,err:String(e).slice(0,200)}); } }));
}
// Pass 2 on those transcripts
await pool(out.pass1.filter(p=>p.text), 3, async p=>{
  const x = await sse("https://openrouter.ai/api/v1/chat/completions",{Authorization:"Bearer "+orKey},{model:"google/gemini-3.8-flash",stream:true,temperature:0.2,reasoning:{effort:"low"},messages:[{role:"system",content:twoPassSys},{role:"user",content:p.text}]});
  out.pass2.push({s:p.s,r:p.r,...x});
});
// One pass via OpenRouter (audio + prompt)
const jobs=[]; for(let r=0;r<REPS;r++) for(const s of samples) jobs.push({s,r});
await pool(jobs, 3, async j=>{
  const b64=fs.readFileSync("/tmp/piebench/audio/"+j.s+".wav").toString("base64");
  const x = await sse("https://openrouter.ai/api/v1/chat/completions",{Authorization:"Bearer "+orKey},{model:"google/gemini-3.8-flash",stream:true,temperature:0.2,reasoning:{effort:"low"},messages:[{role:"system",content:onePassSys},{role:"user",content:[{type:"text",text:"Transcribe and tidy this recording."},{type:"input_audio",input_audio:{data:b64,format:"wav"}}]}]});
  out.one.push({...j,...x});
});
// One pass via Google direct (latency)
await pool(jobs, 3, async j=>{
  const b64=fs.readFileSync("/tmp/piebench/audio/"+j.s+".wav").toString("base64");
  const x = await sse("https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:streamGenerateContent?alt=sse",{"x-goog-api-key":gKey},{systemInstruction:{parts:[{text:onePassSys}]},contents:[{role:"user",parts:[{text:"Transcribe and tidy this recording."},{inline_data:{mime_type:"audio/wav",data:b64}}]}],generationConfig:{temperature:0.2,thinkingConfig:{thinkingLevel:"low"}}});
  out.google.push({...j,...x});
});
fs.writeFileSync("/tmp/piebench/cmp-results.json", JSON.stringify(out,null,1));
const q=(a,p)=>{a=a.filter(x=>x!=null).sort((x,y)=>x-y); return a.length?Math.round(a[Math.min(a.length-1,Math.floor(p*a.length))]):null};
const stat=(name,a)=>console.log(JSON.stringify({name,n:a.length,p50:q(a,.5),p90:q(a,.9),max:q(a,.999)}));
const p1=out.pass1.filter(x=>x.ms!=null); stat("pass1 after release",p1.map(x=>x.ms));
stat("pass2",out.pass2.filter(x=>!x.err).map(x=>x.ms));
const tot=out.pass2.filter(x=>!x.err).map(x=>{const a=p1.find(p=>p.s===x.s&&p.r===x.r); return a? a.ms+x.ms : null}); stat("two-pass total",tot);
stat("one-pass openrouter",out.one.filter(x=>!x.err).map(x=>x.ms));
stat("one-pass google direct",out.google.filter(x=>!x.err).map(x=>x.ms));
console.log("errors", out.pass1.filter(x=>x.err).length, out.pass2.filter(x=>x.err).length, out.one.filter(x=>x.err).length, out.google.filter(x=>x.err).length, (out.one.find(x=>x.err)||out.google.find(x=>x.err)||{}).err||"");
