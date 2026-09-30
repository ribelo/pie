
import fs from "node:fs";
const key = fs.readFileSync("/run/agenix/openrouter-api-key","utf8").trim();
const {system, cases} = JSON.parse(fs.readFileSync("/tmp/piebench/llm-cases.json","utf8"));
const configs = JSON.parse(process.argv[2]);
const reps = +process.argv[3]||3;
const conc = +process.argv[4]||3;
async function one(cfg, text){
  const body = {model:cfg.model, stream:true, temperature:0.2, max_tokens:2000, messages:[{role:"system",content:system},{role:"user",content:text}], ...cfg.extra};
  const t0 = performance.now(); let ttft=null; let out=""; let provider=null; let usage=null;
  const res = await fetch("https://openrouter.ai/api/v1/chat/completions",{method:"POST",headers:{Authorization:"Bearer "+key,"Content-Type":"application/json"},body:JSON.stringify(body)});
  if(!res.ok){return {err:res.status+" "+(await res.text()).slice(0,300), ms:performance.now()-t0}}
  const dec = new TextDecoder(); let buf="";
  for await (const chunk of res.body){ buf+=dec.decode(chunk,{stream:true}); let i;
    while((i=buf.indexOf("\n"))>=0){ const line=buf.slice(0,i).trim(); buf=buf.slice(i+1);
      if(!line.startsWith("data:")) continue; const d=line.slice(5).trim(); if(d==="[DONE]") continue;
      try{ const j=JSON.parse(d); provider=j.provider||provider; if(j.usage) usage=j.usage; const c=j.choices?.[0]?.delta?.content; if(c){ if(ttft===null) ttft=performance.now()-t0; out+=c; } }catch{}
    }}
  return {ms:performance.now()-t0, ttft, out, provider, usage:usage&&{p:usage.prompt_tokens,c:usage.completion_tokens,r:usage.completion_tokens_details?.reasoning_tokens,cost:usage.cost}};
}
const results=[];
for(const cfg of configs){
  const jobs=[]; for(let r=0;r<reps;r++) cases.forEach((c,i)=>jobs.push({i,r,c}));
  let k=0; const rs=[];
  await Promise.all(Array.from({length:conc},async()=>{ while(k<jobs.length){ const j=jobs[k++]; const r=await one(cfg,j.c); rs.push({case:j.i,rep:j.r,...r}); }}));
  results.push({label:cfg.label, rs});
  const ok=rs.filter(r=>!r.err).map(r=>r.ms).sort((a,b)=>a-b); const q=p=>ok.length?Math.round(ok[Math.min(ok.length-1,Math.floor(p*ok.length))]):null;
  const tt=rs.filter(r=>r.ttft!=null).map(r=>r.ttft).sort((a,b)=>a-b); const qt=p=>tt.length?Math.round(tt[Math.min(tt.length-1,Math.floor(p*tt.length))]):null;
  console.log(JSON.stringify({label:cfg.label,n:rs.length,errors:rs.filter(r=>r.err).length,p50:q(.5),p90:q(.9),max:q(.999),ttft50:qt(.5),ttft90:qt(.9),providers:[...new Set(rs.map(r=>r.provider))],reasoningTok:rs.map(r=>r.usage?.r||0).reduce((a,b)=>a+b,0),cost:rs.map(r=>r.usage?.cost||0).reduce((a,b)=>a+b,0).toFixed(5), firstErr:rs.find(r=>r.err)?.err}));
}
fs.writeFileSync(process.argv[5]||"/tmp/piebench/llm-results.json", JSON.stringify(results,null,1));
