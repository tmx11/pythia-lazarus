# Pythia - Git Branching Strategy

## Branch Structure

### Main Branches
- **master** - Stable releases only (v1.0, v2.0, etc.)
- **develop** - Integration branch for completed features

### Feature Branches (Per Architecture Document)
Each implements one unit from [AGENTIC_ARCHITECTURE.md](docs/AGENTIC_ARCHITECTURE.md):

1. **feature/ide-wrapper** → `Pythia.IDE.Wrapper.pas`
   - ToolsAPI abstraction layer
   - File read/write operations
   - Tag: `checkpoint-ide-wrapper`

2. **feature/diff-engine** → `Pythia.Diff.Engine.pas`
   - Diff calculation between old/new text
   - Line-based edit operations
   - Tag: `checkpoint-diff-engine`

3. **feature/agent-parser** → `Pythia.Agent.Parser.pas`
   - Parse AI responses for file operations
   - Extract code blocks and diff markers
   - Tag: `checkpoint-agent-parser`

4. **feature/context-gatherer** → `Pythia.Context.Gatherer.pas`
   - Build AI prompts with file context
   - Gather open files and project structure
   - Tag: `checkpoint-context-gatherer`

5. **feature/agent-executor** → `Pythia.Agent.Executor.pas`
   - Execute parsed operations safely
   - Confirmation dialogs and undo grouping
   - Tag: `checkpoint-agent-executor`

## Workflow

### For Each Feature:
```bash
# Start feature
git checkout -b feature/feature-name develop

# Make changes, commit frequently
git add .
git commit -m "Descriptive message"

# When feature complete
git checkout develop
git merge --no-ff feature/feature-name
git tag checkpoint-feature-name
git push origin develop --tags
```

### Rollback to Checkpoint:
```bash
git checkout checkpoint-feature-name
git checkout -b hotfix/issue-description
```

### Integration Order (Dependencies):
1. ✅ **v1.0-baseline** (current master) - Basic chat working
2. **ide-wrapper** (no dependencies)
3. **diff-engine** (no dependencies)
4. **agent-parser** (no dependencies)
5. **context-gatherer** (depends on: ide-wrapper)
6. **agent-executor** (depends on: all above)
7. **v2.0-release** - Full agentic functionality

## Current State
- Repository: `d:\dev\delphi\pythia2`
- User: tmx@kinecis.com
- Master: Committed as v1.0 baseline (working chat plugin)
- Next: Start with `feature/ide-wrapper`
