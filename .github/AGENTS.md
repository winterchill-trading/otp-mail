# AGENTS.md: otp-mail/.github

`workflows/ci.yml`: runs on push and pull request. It uses the `ox-setup` action and the
`obj-magic` reusable workflow from `winterchill-trading/infra` (both at `@main`). There is no
`pins:` block: the library builds against public packages only.

Steps in order: `dune build`, `dune test`, `dune build @examples/runtest`.

When a step changes, update the job that checks it in the same commit. To check a run, use
`gh run list --limit 1`, and at most one `gh run watch <id> --exit-status`.
