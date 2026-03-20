---
name: sobaka-infra
description: Sobaka.dev self-hosted infrastructure: server layout, networking (Tailscale + Cloudflare Tunnel), Ghost blogs, GitLab CE, backup to Synology NAS via SFTP/rsync, UFW/fail2ban, Home Assistant, VPS management. Use when working on sobaka.dev infra, server configs, networking, backup automation, or self-hosted services.
license: private
compatibility: opencode
---

## Node Map

| Host | OS | Role | Notes |
|------|----|------|-------|
| Jetson Thor | Ubuntu (Nvidia container) | Primary inference server | vLLM, robotics compute |
| toster | Linux | Backup SFTP target + secondary server | Tailscale-accessible |
| Synology NAS | DSM | Long-term backup storage | rsync over Tailscale |
| T480 (headless) | Arch/Ubuntu | Dev workstation | `seva@headless-T480` |
| VPS(es) | Debian/Ubuntu | Public endpoints, Ghost blogs | Hetzner or similar |
| Home | — | Home Assistant host | ESPHome devices, automations |

## Networking Stack
- **Internal mesh**: Tailscale — all nodes reachable by Tailscale hostname
- **Public ingress**: Cloudflare Tunnel — zero open inbound ports on any node
- **DNS**: Cloudflare, domains: sobaka.dev, sevap.ru, axaxa.site
- **Firewall**: UFW default-deny input + fail2ban on all public-facing nodes
- **VPN fallback**: Tailscale exit node if needed

## Public Services & Domains

| URL | Backend | Host |
|-----|---------|------|
| `api.sobaka.dev/v1` | vLLM (Qwen3-Coder-Next) | Jetson Thor → Cloudflare Tunnel |
| `sobaka.dev` | Ghost blog (self-hosted LLM content, benchmarks) | VPS |
| `blog.sevap.ru` | Ghost blog | VPS |
| `axaxa.site` | Ghost blog | VPS |
| `gitlab.sobaka.dev` | GitLab CE | TBD — current setup task |

## GitLab Setup (in progress)
- Target: `gitlab.sobaka.dev`
- Dual storage: local primary + SFTP backup to toster
- Containerized (Docker Compose)
- Cloudflare Tunnel for public access
- Backup to toster via SFTP, then rsync to Synology

## Backup Architecture
```
Service data
    └─► toster (SFTP, incremental, short-term)
            └─► Synology NAS (rsync over Tailscale, long-term)
```
- Scripts in `/opt/backups/` on each node
- Always test restore, not just backup
- GitLab backups: `gitlab-backup create` → SFTP → NAS

## Ghost Blog Management
- All blogs run Ghost in Docker Compose
- MySQL as database backend (not SQLite)
- Nginx or Caddy as reverse proxy inside compose stack
- Theme customizations tracked in git

## Home Assistant
- ESPHome devices: ESP32 nodes for fan/motor/UV lamp control, Bluetooth proxy
- ApexCharts for plant sensor visualization
- Mushroom cards UI
- Energy monitoring dashboard
- Located on dedicated home server / RPi

## Secrets Convention
```bash
# .env (gitignored on every node)
HF_TOKEN=                    # Hugging Face model downloads
CLOUDFLARE_TUNNEL_TOKEN=     # per-service tunnel token
GITLAB_ROOT_PASSWORD=
SFTP_USER=
SFTP_HOST=toster             # Tailscale hostname
```

## Key Paths
```
~/Dev/                        # all docker compose projects
/data/huggingface/            # HF model cache (Jetson Thor)
/opt/backups/                 # backup scripts
/opt/<service>/               # service config files
```

## Security Practices
- fail2ban on SSH (all public nodes)
- SSH key-only auth, no password login
- UFW: only allow ports actually in use
- Cloudflare WAF on public endpoints
- Watchtower disabled — manual image updates (controlled upgrades)
