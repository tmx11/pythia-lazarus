# GitHub Copilot Implementation Reference

## Source

The [copilot.vim](https://github.com/github/copilot.vim) project was the key reference for understanding GitHub Copilot's authentication flow.

## Authentication Flow

Based on `autoload/copilot/client.vim`, the authentication works as follows:

1. **OAuth Device Flow**: Request device code and user code from GitHub
   - Endpoint: `https://github.com/login/device/code`
   - Client ID: `Iv1.b507a08c87ecfe98` (public, same as VS Code)

2. **User Authorization**: User visits verification URL and enters code
   - Opens browser to `https://github.com/login/device`
   - User enters the 8-character code

3. **Token Polling**: Poll GitHub for OAuth access token
   - Endpoint: `https://github.com/login/oauth/access_token`
   - Wait with 5-second intervals
   - Handle `authorization_pending`, `slow_down`, etc.

4. **🔑 Token Exchange** (Critical Step): Exchange OAuth token for Copilot-specific token
   - Endpoint: `https://api.github.com/copilot_internal/v2/token`
   - Headers:
     - `Authorization: token {oauth_token}`
     - `Editor-Version: Delphi/12.0`
     - `Editor-Plugin-Version: pythia/1.0`
   - Response: `{ "token": "..." }`

5. **API Usage**: Use Copilot token for chat completions
   - Endpoint: `https://api.githubcopilot.com/chat/completions`
   - Headers:
     - `Authorization: Bearer {copilot_token}`
     - `Editor-Version: Delphi-12.0`
     - `Editor-Plugin-Version: pythia-1.0`

## Key Insight

The OAuth token alone **cannot** be used directly with the Copilot API - it must be exchanged for a Copilot-specific token first. This is the critical step that VS Code extensions perform behind the scenes.

## Implementation

See [Pythia.GitHub.Auth.pas](../Source/Pythia.GitHub.Auth.pas) for our Delphi implementation:
- `StartDeviceFlow()` - Step 1
- `PollForToken()` - Steps 2-3
- `GetCopilotToken()` - Step 4 (token exchange)
- `GetAuthToken()` - Returns Copilot token for API use

## Related Files

- [Pythia.AI.Client.pas](../Source/Pythia.AI.Client.pas) - Uses Copilot token in `CallGitHubCopilot()`
- [Pythia.SettingsForm.pas](../Source/Pythia.SettingsForm.pas) - GitHub sign-in UI
