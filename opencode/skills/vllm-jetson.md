---
name: vllm-jetson
description: vLLM on Jetson Thor: NVFP4 quants, FLASHINFER backend, Qwen3-Coder-Next, model benchmarking (VLM and coding), ROI modeling for self-hosted inference, opencode/Claude Code integration, sobaka.dev blog posts about inference. Use when working on vLLM config, model serving, benchmarking, inference costs, or coding agent setup.
license: private
compatibility: opencode
---

## Hardware
- **Device**: Jetson Thor (Nvidia)
- **Container**: `ghcr.io/nvidia-ai-iot/vllm:latest-jetson-thor` (Jetson-specific build)
- **Storage**: `/data/` mounted persistent volume for HF cache

## Current Model Stack

| Model | Quant | Port | Tool Parser | Use |
|-------|-------|------|-------------|-----|
| Qwen3-Coder-Next-NVFP4 | NVFP4 | 8868 | qwen3_coder | Coding agent (opencode, Claude Code) |

HuggingFace: `GadflyII/Qwen3-Coder-Next-NVFP4`
Previously benchmarked: Qwen3-VL, Nemotron-Nano (VLM for robotics perception)

## vLLM Docker Compose (key flags)
```yaml
command:
  - vllm
  - serve
  - GadflyII/Qwen3-Coder-Next-NVFP4
  - --host
  - "0.0.0.0"
  - --port
  - "8868"
  - --gpu-memory-utilization
  - "0.8"
  - --enable-auto-tool-choice
  - --tool-call-parser
  - qwen3_coder
  - --served-model-name
  - sobaka-dev
  - --max-model-len
  - "262144"
  - --attention-config.backend
  - FLASHINFER
environment:
  - HF_TOKEN=${HF_TOKEN}
  - HF_HOME=/data/huggingface
  - HF_HUB_CACHE=/data/huggingface/hub
```

## Sampling Parameters (Qwen3-Coder-Next optimal)
- temperature: 1.0, top_p: 0.95, top_k: 40

## API Endpoints
- **Internal** (Tailscale): `http://jetson-thor:8868/v1`
- **Public** (Cloudflare Tunnel): `https://api.sobaka.dev/v1`
- Model name for API calls: `sobaka-dev`

## opencode Config
```json
{
  "provider": {
    "openai": {
      "options": {
        "baseURL": "https://api.sobaka.dev/v1",
        "apiKey": "sk-..."
      }
    }
  },
  "model": "openai/sobaka-dev"
}
```
Claude Code env vars: `ANTHROPIC_BASE_URL`, `ANTHROPIC_AUTH_TOKEN`, `ANTHROPIC_MODEL=sobaka-dev`

## Benchmarking & Blog
- Benchmark suite: `~/Dev/benchmarks/`
- Key metrics: tokens/sec, TTFT, tool-call success rate, cost/1M tokens vs cloud
- ROI modeling: self-hosted vs Anthropic/OpenAI cloud API
- Published to `sobaka.dev` with visualization tooling

## Known Gotchas
- NVFP4 requires Jetson-specific vLLM image — standard vLLM image won't work
- `--max-model-len 262144` = 256k, NVFP4 VRAM constraint
- FLASHINFER required for Jetson Thor (not FLASH_ATTN)
- HF_HOME must point to persistent `/data/` — container restarts wipe default cache
- Tool calling: `qwen3_coder` parser converts model's XML output → OpenAI JSON format

## WBSO / S&O (Dutch R&D Tax)
- Jetson Thor inference R&D qualifies for WBSO S&O hours
- Track hours on benchmarking and model research for 2026 application
