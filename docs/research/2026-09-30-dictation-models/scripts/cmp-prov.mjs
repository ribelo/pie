
import fs from "node:fs";
const orKey = fs.readFileSync("/run/agenix/openrouter-api-key","utf8").trim();
const {onePassSys} = JSON.parse(fs.readFileSync("/tmp/piebench/cmp-prompts.json","utf8"));
const samples = ["s1","s2","s3","s4","s5","s6","s7","s8","s9","s10"];
const pool = async (jobs, n, fn) => { let k=0; await Promise.all(Array.from({length:n}, async()=>{ while(k<jobs.length){ const j=jobs[k++]; await fn(j);} })); };
async function call(provider, s){
  const b64=fs.readFileSync("/tmp/piebench/audio/"+s+".wav").toString("base64");
  const t0=performance.now();
  const res = await fetch("https://openrouter.ai/api/v1/chat/completions",{method:"POST",headers:{Authorization:"Bearer "+orKey,"Content-Type":"application/json"},body:JSON.stringify({model:"google/gemini-3.8-flash",temperature:0.2,reasoning:{effort:"low"},provider:{order:[provider],allow_fallbacks:false},messages:[{role:"system",content:onePassSys},{role:"user",content:[{type:"text",text:"Transcribe and tidy this recording."},{type:"input_audio",input_audio:{data:b64,format:"wav"}}]}]}),signal:AbortSignal.timeout(60000)});
  const j = await res.json(); return {ms:performance.now()-t0, ok:res.ok, prov:j.provider, err: res.ok?undefined:JSON.stringify(j).slice(0,200)};
}
const q=(a,p)=>{a=a.sort((x,y)=>x-y); return a.length?Math.round(a[Math.min(a.length-1,Math.floor(p*a.length))]):null};
for (const prov of ["Google AI Studio","Google"]) {
  const jobs=[]; for(let r=0;r<3;r++) for(const s of samples) jobs.push(s);
  const rs=[]; await pool(jobs,3,async s=>{ rs.push(await call(prov,s)); });
  const ok=rs.filter(r=>r.ok).map(r=>r.ms);
  console.log(JSON.stringify({prov,n:rs.length,err:rs.filter(r=>!r.ok).length,p50:q(ok,.5),p90:q(ok,.9),max:q(ok,.999),firstErr:rs.find(r=>!r.ok)?.err}));
}