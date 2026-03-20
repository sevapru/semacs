# Seva / Sobaka.dev — Global Agent Rules

## Identity & Language
- Respond in the same language as the user's message (Russian or English)
- User is Vsevolod (Seva), senior engineer with background in robotics (ITMO University)
- Based in Amsterdam, Netherlands; works at Prosus AI Research Lab as self-employed contractor (eenmanszaak: Sobaka)
- No need to explain basics — be direct, skip preamble, no excessive caveats

## Code Style

### Python
- Type hints always, f-strings, pathlib over os.path
- Pydantic for data validation/serialization, dataclasses for simple structs
- async/await for I/O-bound work, especially in FastAPI and inference pipelines
- `uv` for package management (faster than pip), pin versions in pyproject.toml
- Never use `os.system()` — use `subprocess.run()` with explicit args list

### Shell / Bash
- `set -euo pipefail` at top of every script
- Quote all variables: `"${VAR}"`, not `$VAR`
- Prefer explicit over clever; readability over brevity
- Use `#!/usr/bin/env bash`, not `#!/bin/bash`

### Docker / Compose
- Multi-stage builds to minimize image size
- Pin image tags — never `:latest` in production configs
- Non-root user inside containers where possible
- `.dockerignore` always present

### YAML / Config
- 2-space indent
- Explicit types where ambiguous
- Anchors (`&`) for repeated blocks in compose files

### Comments
- Explain *why*, not *what*
- TODO comments include context: `# TODO(seva): ...`

## Infrastructure Preferences
- Self-hosted everything — avoid cloud-managed services
- Docker Compose for single-node, keep it simple
- Tailscale for internal mesh networking between all nodes
- Cloudflare Tunnel for public ingress — no open inbound ports
- UFW default-deny + fail2ban on public-facing nodes
- Backups: always verify restore, not just backup existence
- Secrets via `.env` files (gitignored), never hardcoded
- Logs to stdout — let container runtime handle rotation

## API / Service Conventions
- FastAPI for Python HTTP services
- OpenAI-compatible API format for any LLM-serving endpoint
- Health check endpoint `/health` or `/v1/models` on every service
- Rate limiting on public endpoints

## Git Conventions
- Conventional commits: `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`
- Small focused commits, not "WIP" dumps
- `.env.example` always committed alongside `.env` (gitignored)
- Never force-push to main/master

## What NOT to do
- Don't suggest cloud-managed services (AWS RDS, managed K8s, etc.) — prefer self-hosted
- Don't add new dependencies without asking first
- Don't reformat code that wasn't part of the task
- Don't truncate long outputs — show full content
- Don't use `:latest` image tags in any config file
- Don't suggest TypeScript/Node for backend unless explicitly asked — Python preferred
- Don't over-engineer: simple working solution > elegant complex one
