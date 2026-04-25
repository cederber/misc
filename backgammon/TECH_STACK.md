# Technology Stack Decisions

This document captures the technology evaluation and choices for the backgammon match recorder project.

## Backend: Rust + Axum

**Choice:** [Axum](https://github.com/tokio-rs/axum) as the web framework, with [serde](https://serde.rs/) for serialization and [sqlx](https://github.com/launchbadge/sqlx) for database access.

**Why Axum:**
- Built by the Tokio team; the de facto default for new Rust web projects
- Composable via Tower middleware, tightly integrated with tokio/hyper
- Shares the Tower service foundation with tonic (gRPC), enabling both JSON REST and gRPC on the same port
- Strong ecosystem: production-ready, actively maintained, large community

**Alternatives considered:**
- *Actix Web:* Slightly faster under extreme load, but the difference is irrelevant for this use case. Axum has better ecosystem integration.
- *Rocket:* More opinionated/batteries-included, but smaller ecosystem footprint and slower release cadence.

**Known tradeoffs:**
- Rust compile times are slow (especially incremental rebuilds)
- Smaller middleware ecosystem than Node.js or Go — not a problem for this project's scope

## Frontend Language: TypeScript

**Choice:** TypeScript over plain JavaScript.

**Rationale:** ~80%+ adoption among professional JS developers. First-class support in all major frameworks. Static typing catches bugs at compile time and improves editor tooling. There is no reason to start a new project in plain JS.

**Alternatives to JavaScript/TypeScript considered:**
- *Rust/WASM (Leptos, Dioxus):* Viable for simple UIs, but drag-and-drop requires painful JS interop. Ecosystem is thin for DOM-heavy interaction.
- *Blazor (C#/WASM):* Most production-ready WASM option, but large download size (~5-10 MB) and only makes sense for .NET teams.
- *Flutter Web:* Renders to its own canvas — good for the board, awkward for text UI and forms.
- *Elm / PureScript / ReScript:* Small communities, not mainstream enough.

**Verdict:** TypeScript/JavaScript is the clear practical default for browser apps requiring drag-and-drop, text panels, and forms.

## UI Framework: React

**Choice:** [React](https://react.dev/) for the UI framework.

**Rationale:**
- Largest ecosystem and best library support for canvas integration
- Official Konva.js bindings (`react-konva`) for the board rendering
- Straightforward pattern: React components for text panels and forms, canvas board mounted inside a React component via refs
- TanStack Query has first-class React support

**Alternatives considered:**
- *Vue 3:* Solid alternative with excellent TypeScript support. Has `vue-konva` bindings. Slightly smaller ecosystem than React.
- *Svelte:* Lean compiled output, but thinner ecosystem for canvas integration libraries.
- *SolidJS:* Excellent performance with fine-grained reactivity, but smaller ecosystem.
- *Angular:* Overkill for this project's scope.

## Board Rendering: Konva.js (react-konva)

**Choice:** [Konva.js](https://konvajs.org/) via [react-konva](https://github.com/konvajs/react-konva).

**Rationale:**
- Purpose-built for interactive 2D canvas with built-in drag-and-drop, hit detection, layering, and event handling
- Excellent fit for rendering and interacting with a backgammon board
- Official React bindings provide a declarative component API

**Alternatives considered:**
- *Fabric.js:* Good drag-and-drop, but more oriented toward design/drawing tools.
- *Pixi.js:* WebGL-first renderer designed for games. More power than needed; adds complexity.
- *Raw Canvas API:* Would require reimplementing hit detection and drag-and-drop from scratch.
- *SVG + D3:* D3 is for data visualization. SVG alone works but managing drag-and-drop and state gets verbose.

## State Management: useReducer + TanStack Query

**Choice:** React's built-in `useReducer` for local game state, [TanStack Query](https://tanstack.com/query) for server state.

**Rationale:**
- Two-layer approach separates concerns: game logic (board positions, move history) is local state; persistence (fetching/posting game logs) is server state
- `useReducer` is a natural fit for game state — actions map cleanly to moves
- TanStack Query handles caching, refetching, and loading states for API communication
- Avoids over-engineering with Redux for a single-user board game recorder

## Build Tool: Vite

**Choice:** [Vite](https://vitejs.dev/).

**Rationale:** The unambiguous default for new frontend projects. Fast dev server with hot module replacement, used by all major framework scaffolding tools. Webpack is legacy/maintenance-mode.

## Client-Server Communication

### Phase 1: JSON REST

Simple JSON over HTTP via Axum endpoints and TanStack Query on the client. No additional tooling needed — this is the native format for both Axum (with serde) and browser fetch APIs.

### Phase 2 (future): Proto-based via ConnectRPC

**Planned approach:** [ConnectRPC](https://connectrpc.com/) end-to-end.

**Why ConnectRPC over raw gRPC:**
- Browsers cannot speak native gRPC (no HTTP/2 trailer support). The two viable options are gRPC-Web and ConnectRPC.
- ConnectRPC uses standard HTTP semantics (JSON or binary) that work in any browser — no proxy (e.g., Envoy) needed.
- A single ConnectRPC server speaks three protocols simultaneously: Connect, gRPC, and gRPC-Web.
- The TypeScript client (`@connectrpc/connect-web`) has a TanStack Query integration (`@connectrpc/connect-query`) that generates typed React hooks directly from `.proto` files — a natural fit with our existing stack.

**Rust server side:**
- `connectrpc` + `connectrpc-axum` — Tower-based, integrates with our existing Axum server
- Alternatively, `tonic` (standard Rust gRPC) + `tonic-web` middleware for gRPC-Web support
- Both share Tower middleware with Axum, so auth/tracing/rate-limiting can be reused
- Can run on the same port as JSON REST endpoints

**TypeScript client side:**
- `@connectrpc/connect-web` for the client transport
- `@connectrpc/connect-query` for TanStack Query integration (generated `useQuery`/`useMutation` hooks)
- Protobuf-ES (`@bufbuild/protobuf`) for type generation from `.proto` files
- Code generation via `buf generate`

**Migration path:**
- JSON REST and ConnectRPC endpoints can coexist on the same Axum server
- Migrate endpoint-by-endpoint, no big-bang cutover required
