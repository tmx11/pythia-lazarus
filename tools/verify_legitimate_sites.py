#!/usr/bin/env python3
"""
Verify you're connecting to legitimate AI provider websites
Security check for API endpoints
"""

import requests
import ssl
from urllib.parse import urlparse

def check_ssl_cert(url, expected_org):
    """Check SSL certificate of a URL"""
    print(f"\n🔒 Checking SSL Certificate: {url}")
    print("-" * 80)
    
    try:
        response = requests.get(url, timeout=10)
        
        # Get SSL cert info (simplified check)
        parsed = urlparse(url)
        hostname = parsed.netloc
        
        print(f"✅ Connection successful")
        print(f"   Status: {response.status_code}")
        print(f"   Server: {response.headers.get('Server', 'Unknown')}")
        
        # Check if it's the actual API endpoint
        if 'openai.com' in hostname or 'anthropic.com' in hostname:
            print(f"✅ Verified legitimate domain: {hostname}")
        else:
            print(f"⚠️  Domain: {hostname} - Verify this is correct!")
        
        return True
        
    except Exception as e:
        print(f"❌ Connection failed: {e}")
        return False

def main():
    print("="*80)
    print(" LEGITIMATE AI PROVIDER VERIFICATION")
    print("="*80)
    
    print("\n📋 LEGITIMATE SITES:")
    print("-" * 80)
    
    providers = {
        "OpenAI API": {
            "endpoint": "https://api.openai.com/v1/models",
            "website": "https://platform.openai.com",
            "chat": "https://chat.openai.com",
            "docs": "https://platform.openai.com/docs"
        },
        "Anthropic Claude API": {
            "endpoint": "https://api.anthropic.com/v1/messages",
            "console": "https://console.anthropic.com",
            "chat": "https://claude.ai",
            "docs": "https://docs.anthropic.com"
        }
    }
    
    for provider, urls in providers.items():
        print(f"\n{provider}:")
        for purpose, url in urls.items():
            print(f"   • {purpose.capitalize()}: {url}")
    
    print("\n" + "="*80)
    print(" ⚠️  SECURITY WARNINGS")
    print("="*80)
    
    print("\n❌ SUSPICIOUS SITES - DO NOT USE:")
    print("   • use.ai - NOT affiliated with Claude/Anthropic")
    print("   • Any site asking for API keys that's not the official console")
    print("   • Lookalike domains (cIaude.ai with capital i, etc.)")
    
    print("\n✅ HOW TO STAY SAFE:")
    print("   1. Always type URLs manually (don't click links)")
    print("   2. Check SSL certificate (🔒 lock icon in browser)")
    print("   3. Verify URL exactly matches official domains")
    print("   4. Never enter API keys on third-party sites")
    
    print("\n" + "="*80)
    print(" TESTING API ENDPOINTS")
    print("="*80)
    
    # Test OpenAI endpoint
    check_ssl_cert("https://api.openai.com/v1/models", "OpenAI")
    
    # Test Anthropic endpoint  
    check_ssl_cert("https://api.anthropic.com/v1/messages", "Anthropic")
    
    print("\n" + "="*80)
    print(" WHAT TO DO IF YOU ENTERED CREDENTIALS ON WRONG SITE")
    print("="*80)
    print("\n🚨 If you entered API keys or passwords on 'use.ai' or any suspicious site:")
    print("   1. IMMEDIATELY revoke/delete those API keys")
    print("   2. OpenAI: https://platform.openai.com/api-keys")
    print("   3. Anthropic: https://console.anthropic.com/settings/keys")
    print("   4. Generate new API keys")
    print("   5. Change your account password")
    print("   6. Check billing for unauthorized usage")
    
    print("\n" + "="*80)
    print(" CLAUDE CHAT (Free) vs CLAUDE API (Paid)")
    print("="*80)
    print("\n🆓 Claude Chat (claude.ai):")
    print("   • Free tier available")
    print("   • Web interface like ChatGPT")
    print("   • No API key needed")
    print("   • For personal conversations")
    
    print("\n💰 Claude API (console.anthropic.com):")
    print("   • For developers/programmers")
    print("   • Pay-as-you-go")
    print("   • Requires API key")
    print("   • For apps like Pythia")

if __name__ == '__main__':
    main()
