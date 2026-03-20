---
name: robotics-unitree
description: Unitree G1 humanoid robot development: OmniPlex full-duplex conversational AI architecture, IEEE paper writing, ROS2, MuJoCo/Isaac Sim/Isaac Lab, motion control (MPC-residual hybrid), Jetson Thor on-robot inference, SO-101 arms, LeRobot. Use when working on robotics code, OmniPlex paper, robot control, simulation, or hackathon planning.
license: private
compatibility: opencode
---

## Hardware Fleet

| Robot | Type | Status | Notes |
|-------|------|--------|-------|
| Unitree G1 | Humanoid | Active | OmniPlex platform, conference guide (Darwin persona) |
| Unitree Go2 Pro | Quadruped | Reference | Security research (UniPwn BLE), previous project |
| SO-101 arms | Manipulation arm | Active | RoboFoodChain hackathon, LeRobot |

## OmniPlex Project
Full-duplex multimodal conversational AI for humanoid robots on Unitree G1 + Jetson Thor.

### Architecture
- **Full-duplex audio**: simultaneous send/receive, no push-to-talk
- **Multimodal perception**: vision + audio fusion
- **Motion control**: MPC-residual hybrid controller
- **Low-latency inference**: latent token embedding for action prediction
- **Compute**: Jetson Thor co-located on robot

### Paper
- Target: IEEE conference/journal
- Status: active writing, architecture diagrams done (Prosus AI Team palette)
- Collaborators: Prosus AI Research Lab, Amsterdam
- Recent work: MPC-residual hybrid control section, latent token embedding section

### Latency Budget
- End-to-end conversational response: < 200ms
- Audio VAD detection: < 50ms
- Motion command generation: < 100ms

## Software Stack
- **ROS2**: robot middleware, sensor fusion
- **Simulation**: MuJoCo (primary), Isaac Sim, Isaac Lab (RL training)
- **RL/Control**: Isaac Lab for locomotion training
- **Languages**: Python (AI/perception), C++ (real-time control loops)
- **Inference**: vLLM on Jetson Thor for on-robot LLM
- **Manipulation**: LeRobot framework (SO-101 arms)

## RoboFoodChain Hackathon
- Format: 48-hour robotics hackathon
- Hardware: SO-101 arms + LeRobot
- Target date: Q2 2026
- Venue: Prosus AI House, Amsterdam
- Status: proposal phase

## Darwin Persona (OKX Claim AI Summit)
- Unitree G1 deployed as conference guide at OKX event in Lisbon
- Persona: "Darwin" — friendly AI robot guide
- Interaction: conversational, directional, event info

## Development Conventions
- Safety first: joint limit checks before every motion command
- Simulation before hardware: validate in MuJoCo, then real robot
- Use `unitree_sdk2` for low-level robot control
- ROS2 nodes should be well-typed, use `rclpy` lifecycle nodes
- Log all robot state at 100Hz minimum during experiments

## Prosus AI Context
- Organization: Prosus AI Research Lab, Amsterdam
- Contract: eenmanszaak Sobaka
- Design palette: Prosus AI Team brand colors (for diagrams, LaTeX figures)
- Paper formatting: IEEE LaTeX template
