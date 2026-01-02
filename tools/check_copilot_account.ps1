#!/usr/bin/env powershell
# Check GitHub Copilot subscription status

Write-Host "="*80
Write-Host "GITHUB COPILOT ACCOUNT INFO"
Write-Host "="*80

Write-Host "`n📋 How to check your GitHub Copilot plan:"
Write-Host "   1. Go to: https://github.com/settings/copilot"
Write-Host "   2. Look for 'Copilot plan' section"
Write-Host ""

Write-Host "💰 GITHUB COPILOT PRICING TIERS:"
Write-Host "-"*80

Write-Host "`n🆓 FREE TIER (Limited):"
Write-Host "   • Available to verified students, teachers, open source maintainers"
Write-Host "   • 2,000 code completions/month"
Write-Host "   • 50 chat messages/month"
Write-Host "   • Access to Claude Sonnet & GPT-4"

Write-Host "`n💎 COPILOT PRO (`$10/month):"
Write-Host "   • Unlimited code completions"
Write-Host "   • Unlimited chat messages"
Write-Host "   • Access to Claude Sonnet & GPT-4"
Write-Host "   • Faster responses"

Write-Host "`n🏢 COPILOT BUSINESS/ENTERPRISE:"
Write-Host "   • Through your organization/employer"
Write-Host "   • Unlimited usage"
Write-Host "   • Admin controls"

Write-Host "`n" + "="*80
Write-Host "WHY YOU'RE NOT PAYING ANTHROPIC DIRECTLY"
Write-Host "="*80

Write-Host "`n🔄 The Middleman Model:"
Write-Host "   YOU → GitHub Copilot → Anthropic/OpenAI"
Write-Host ""
Write-Host "   • You pay GitHub (or use free tier)"
Write-Host "   • GitHub pays Anthropic for Claude API access"
Write-Host "   • GitHub handles billing, infrastructure, rate limits"
Write-Host "   • You get simple flat-rate pricing"

Write-Host "`n" + "="*80
Write-Host "YOUR PYTHIA PLUGIN VS GITHUB COPILOT"
Write-Host "="*80

Write-Host "`n🔧 Pythia Plugin (Your Delphi IDE):"
Write-Host "   • Direct API access: YOU → OpenAI/Anthropic"
Write-Host "   • You pay OpenAI/Anthropic directly (pay-per-use)"
Write-Host "   • No middleman"
Write-Host "   • Need to add your own API key"
Write-Host "   • Need to add credits to your API account"

Write-Host "`n💬 GitHub Copilot (VS Code - RIGHT NOW):"
Write-Host "   • Bundled service: YOU → GitHub → Anthropic"
Write-Host "   • You pay GitHub fixed rate (or free tier)"
Write-Host "   • GitHub handles API costs"
Write-Host "   • Already authenticated via GitHub account"
Write-Host "   • No API key needed"

Write-Host "`n" + "="*80
Write-Host "CHECK YOUR GITHUB ACCOUNT"
Write-Host "="*80

Write-Host "`nTo see which plan you're on:"
Write-Host "   1. Open: https://github.com/settings/copilot"
Write-Host "   2. Or run: gh auth status (if GitHub CLI installed)"
Write-Host ""

# Try to check if GitHub CLI is installed
$ghInstalled = Get-Command gh -ErrorAction SilentlyContinue

if ($ghInstalled) {
    Write-Host "🔍 Checking GitHub CLI authentication..."
    Write-Host ""
    gh auth status
    Write-Host ""
    Write-Host "For Copilot subscription details, visit:"
    Write-Host "https://github.com/settings/copilot"
} else {
    Write-Host "💡 GitHub CLI not installed."
    Write-Host "   Visit https://github.com/settings/copilot to see your plan"
}

Write-Host "`n" + "="*80
Write-Host "SUMMARY"
Write-Host "="*80
Write-Host "`n✅ You're using Claude Sonnet through GitHub Copilot (in VS Code)"
Write-Host "✅ This is DIFFERENT from OpenAI/Anthropic API direct access"
Write-Host "✅ GitHub acts as middleman, you pay them (not Anthropic)"
Write-Host "✅ For Pythia plugin, you need separate API keys (direct access)"
Write-Host ""
