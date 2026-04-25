# Backgammon Match Recorder

A web application for recording backgammon matches in progress and exporting them in a standard format for later analysis.

## Features (Planned)

- Graphical backgammon board with drag-and-drop piece movement
- Text-based game log display and entry
- Match persistence via backend API
- Export to standard backgammon notation formats
- Desktop/laptop browser interface

## Tech Stack

See [TECH_STACK.md](TECH_STACK.md) for detailed technology choices and rationale.

| Layer | Technology |
|---|---|
| Backend | Rust + Axum |
| Frontend language | TypeScript |
| UI framework | React |
| Board rendering | Konva.js (react-konva) |
| State management | useReducer + TanStack Query |
| Build tool | Vite |
| Client/server communication | JSON REST initially, ConnectRPC (proto-based) later |
