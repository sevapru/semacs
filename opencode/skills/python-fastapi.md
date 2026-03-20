---
name: python-fastapi
description: FastAPI service patterns for AI/LLM proxies, inference APIs, OpenAI-compatible endpoints: async handlers, streaming responses, Pydantic models, middleware, Docker deployment. Use when building or modifying Python backend services, API proxies, or inference serving wrappers.
license: private
compatibility: opencode
---

## Project Structure
```
src/
  main.py           # FastAPI app, lifespan, router includes
  routers/
    v1/
      chat.py       # /v1/chat/completions
      models.py     # /v1/models
  models/           # Pydantic schemas
  services/         # business logic, inference clients
  middleware/       # auth, rate limiting, logging
  config.py         # settings via pydantic-settings
pyproject.toml
Dockerfile
docker-compose.yml
.env.example
```

## FastAPI App Template
```python
from contextlib import asynccontextmanager
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

@asynccontextmanager
async def lifespan(app: FastAPI):
    # startup
    yield
    # shutdown

app = FastAPI(title="Sobaka API", lifespan=lifespan)
app.add_middleware(CORSMiddleware, allow_origins=["*"], ...)
```

## OpenAI-Compatible Streaming Pattern
```python
from fastapi.responses import StreamingResponse
import httpx

async def stream_completion(request: ChatRequest):
    async with httpx.AsyncClient() as client:
        async with client.stream("POST", UPSTREAM_URL, json=payload) as r:
            async for chunk in r.aiter_text():
                yield chunk

@router.post("/v1/chat/completions")
async def chat(request: ChatRequest):
    return StreamingResponse(
        stream_completion(request),
        media_type="text/event-stream"
    )
```

## Pydantic Settings
```python
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    upstream_url: str = "http://jetson-thor:8868/v1"
    api_key: str
    model_name: str = "sobaka-dev"

    class Config:
        env_file = ".env"

settings = Settings()
```

## Auth Middleware (Bearer Token)
```python
from fastapi import Security, HTTPException
from fastapi.security import HTTPBearer

security = HTTPBearer()

async def verify_token(credentials = Security(security)):
    if credentials.credentials != settings.api_key:
        raise HTTPException(status_code=401)
    return credentials
```

## Health Check (required on every service)
```python
@app.get("/health")
async def health():
    return {"status": "ok", "model": settings.model_name}
```

## Dockerfile (multi-stage)
```dockerfile
FROM python:3.12-slim as builder
WORKDIR /app
RUN pip install uv
COPY pyproject.toml .
RUN uv pip install --system -r pyproject.toml

FROM python:3.12-slim
WORKDIR /app
COPY --from=builder /usr/local/lib/python3.12 /usr/local/lib/python3.12
COPY src/ ./src/
CMD ["uvicorn", "src.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

## Common Patterns
- Use `httpx.AsyncClient` for upstream calls, not `requests`
- Use `asyncio.TaskGroup` for parallel upstream requests
- Type everything with Pydantic — no raw dicts in handlers
- Log request ID through entire call chain for debugging
- Return proper HTTP status codes, not always 200 with error in body
