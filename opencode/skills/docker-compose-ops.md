---
name: docker-compose-ops
description: Docker Compose conventions for Sobaka.dev: service structure, health checks, resource limits, volume layout, Cloudflare Tunnel sidecar, env file patterns, Jetson Thor GPU passthrough, update strategy. Use when writing or editing any docker-compose.yml for self-hosted services.
license: private
compatibility: opencode
---

## Required in Every Service
```yaml
services:
  my-service:
    image: owner/image:1.2.3          # pinned — never :latest in production
    container_name: my-service        # explicit for docker logs/exec
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:PORT/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 60s               # give slow services time to start
    deploy:
      resources:
        limits:
          memory: 2g
    environment:
      - VAR=${VAR}                     # always from .env, never hardcoded
    logging:
      driver: json-file
      options:
        max-size: "50m"
        max-file: "3"
    networks:
      - internal
```

## Volume Layout Convention
```
/data/                    # persistent data (survives recreate)
  huggingface/            # HF model cache (Jetson)
  gitlab/                 # GitLab data
  ghost/                  # Ghost blog content
  <service>/              # per-service persistent data
/opt/<service>/           # config files (can live in repo)
```

## Cloudflare Tunnel Sidecar Pattern
```yaml
  cloudflared:
    image: cloudflare/cloudflared:2025.1.0   # pin version
    container_name: cloudflared-<service>
    restart: unless-stopped
    command: tunnel --no-autoupdate run
    environment:
      - TUNNEL_TOKEN=${CLOUDFLARE_TUNNEL_TOKEN}
    depends_on:
      my-service:
        condition: service_healthy
    networks:
      - internal
```

## Jetson Thor GPU Passthrough
```yaml
  vllm-coder:
    image: ghcr.io/nvidia-ai-iot/vllm:latest-jetson-thor
    runtime: nvidia                    # nvidia container runtime
    environment:
      - NVIDIA_VISIBLE_DEVICES=all
      - NVIDIA_DRIVER_CAPABILITIES=compute,utility
```

## Network Convention
```yaml
networks:
  internal:
    driver: bridge
    name: <stack>-internal
```
- One custom bridge per compose stack
- Internal services NOT exposed on host ports (use Tailscale or Tunnel)
- Tailscale: use `network_mode: host` on the tailscale container only

## .env File Pattern
```bash
# .env.example  ← committed to git (empty values)
# .env          ← gitignored (real values)

SERVICE_API_KEY=
HF_TOKEN=
CLOUDFLARE_TUNNEL_TOKEN=
POSTGRES_PASSWORD=
GITLAB_ROOT_PASSWORD=
SFTP_USER=
SFTP_HOST=toster
```

## Update Strategy (no Watchtower)
```bash
# Manual controlled update
docker compose pull
docker compose up -d --no-deps --build <service>
# Verify health before moving on
docker compose ps
docker compose logs -f <service>
```

## Useful Commands
```bash
docker compose up -d --build                 # rebuild and start
docker compose logs -f service-name          # follow logs
docker compose exec service bash             # shell
docker compose down -v                       # destroy including volumes (careful!)
docker system prune -f                       # clean dangling images
```

## Common Patterns by Service Type

### Ghost Blog
```yaml
  ghost:
    image: ghost:5.x.x-alpine
    depends_on:
      db:
        condition: service_healthy
    volumes:
      - /data/ghost:/var/lib/ghost/content
```

### GitLab CE
```yaml
  gitlab:
    image: gitlab/gitlab-ce:17.x.x-ce.0
    shm_size: '256m'
    volumes:
      - /data/gitlab/config:/etc/gitlab
      - /data/gitlab/logs:/var/log/gitlab
      - /data/gitlab/data:/var/opt/gitlab
```
