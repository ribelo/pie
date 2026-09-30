
import fs from "node:fs";
import { execFileSync } from "node:child_process";
const auth = JSON.parse(fs.readFileSync(process.env.HOME+"/.codex/auth.json","utf8"));
const tok = auth.tokens.access_token;
const model = process.argv[2]||"gpt-live-transcribe";
const sample = process.argv[3]||"s1";
const realtimeFeed = process.argv[4]==="realtime";
const pcm = execFileSync("ffmpeg",["-loglevel","error","-i","/tmp/piebench/audio/"+sample+".wav","-f","s16le","-ar","24000","-ac","1","-"],{maxBuffer:1e8});
const url = "wss://api.openai.com/v1/realtime?intent=transcription";
const ws = new WebSocket(url, {headers:{Authorization:"Bearer "+tok, originator:"codex_cli_rs"}});
let tCommit=null, t0=performance.now(), text="", firstDelta=null;
const done = new Promise((res)=>{
ws.onmessage = async (ev)=>{ const m = JSON.parse(ev.data);
  if (m.type==="session.created" || m.type==="transcription_session.created") {
    ws.send(JSON.stringify({type:"session.update", session:{type:"transcription", audio:{input:{format:{type:"audio/pcm",rate:24000}, turn_detection:null, transcription:{model, language:"pl"}}}}}));
  } else if (m.type==="session.updated") {
    const chunk = 24000*2/10; // 100 ms
    for (let i=0;i<pcm.length;i+=chunk){ ws.send(JSON.stringify({type:"input_audio_buffer.append", audio: pcm.subarray(i,i+chunk).toString("base64")})); if(realtimeFeed) await new Promise(r=>setTimeout(r,100)); }
    tCommit=performance.now(); ws.send(JSON.stringify({type:"input_audio_buffer.commit"}));
  } else if (m.type==="conversation.item.input_audio_transcription.delta") { if(firstDelta===null) firstDelta=performance.now()-t0; }
  else if (m.type==="conversation.item.input_audio_transcription.completed") { text=m.transcript; res(); }
  else if (m.type==="error") { console.log("ERR", JSON.stringify(m.error)); res(); }
};
ws.onerror=(e)=>{console.log("wserr", e.message); res();};
ws.onclose=(e)=>{ res(); };
});
await done; ws.close();
console.log(JSON.stringify({model, sample, afterCommitMs: tCommit? Math.round(performance.now()-tCommit):null, totalMs:Math.round(performance.now()-t0), text}));
