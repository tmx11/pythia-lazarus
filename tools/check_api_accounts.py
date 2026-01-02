#!/usr/bin/env python3
"""
Check OpenAI and Anthropic API account status
Shows usage, limits, and billing information
"""

import os
import sys
import json
import configparser
import requests
from datetime import datetime

def get_config_path():
    appdata = os.environ.get('APPDATA')
    config_file = os.path.join(appdata, 'Pythia', 'pythia.ini')
    return config_file

def read_config():
    config_file = get_config_path()
    if not os.path.exists(config_file):
        print(f"ERROR: Config file not found: {config_file}")
        sys.exit(1)
    
    config = configparser.ConfigParser()
    config.read(config_file)
    
    return {
        'openai': config.get('API', 'OpenAIKey', fallback=''),
        'anthropic': config.get('API', 'AnthropicKey', fallback='')
    }

def check_openai_account(api_key):
    """Check OpenAI API account status and usage"""
    print("\n" + "="*80)
    print("OPENAI API ACCOUNT STATUS")
    print("="*80)
    
    if not api_key:
        print("❌ No OpenAI API key configured")
        return
    
    print(f"API Key: {api_key[:20]}...{api_key[-5:]}")
    
    # Try to get usage/billing info
    headers = {
        "Authorization": f"Bearer {api_key}"
    }
    
    print("\n📊 Checking Account Status...")
    print("-" * 80)
    
    # Test with minimal request to see error details
    test_url = "https://api.openai.com/v1/chat/completions"
    test_payload = {
        "model": "gpt-3.5-turbo",
        "messages": [{"role": "user", "content": "test"}],
        "max_tokens": 5
    }
    
    try:
        response = requests.post(test_url, headers=headers, json=test_payload, timeout=10)
        
        if response.status_code == 200:
            print("✅ API Key is ACTIVE and has available credits")
            print("   You can make API calls successfully")
        elif response.status_code == 401:
            print("❌ API Key is INVALID")
            print("   Error: Authentication failed")
        elif response.status_code == 429:
            error_data = response.json()
            error_type = error_data.get('error', {}).get('type', '')
            error_msg = error_data.get('error', {}).get('message', '')
            
            if 'insufficient_quota' in error_type:
                print("❌ INSUFFICIENT QUOTA / NO CREDITS")
                print("\n📋 Issue Details:")
                print("   - Your OpenAI API account has NO available credits")
                print("   - ChatGPT Plus subscription does NOT include API credits")
                print("   - These are separate products with separate billing")
                
                print("\n💡 Solutions:")
                print("   1. Go to: https://platform.openai.com/settings/organization/billing")
                print("   2. Click 'Add payment method'")
                print("   3. Add at least $5-10 in credits")
                print("   4. API costs: ~$0.03 per 1K tokens (GPT-4)")
                print("      Example: ~$0.30 for 100 messages")
                
                print("\n💰 Pricing Info:")
                print("   - GPT-4: $0.03/1K input tokens, $0.06/1K output tokens")
                print("   - GPT-3.5-Turbo: $0.0015/1K input, $0.002/1K output")
                print("   - Pay-as-you-go (no monthly fee)")
                
            elif 'rate_limit' in error_type:
                print("⚠️ RATE LIMIT EXCEEDED")
                print("   Too many requests - wait a moment and try again")
            else:
                print(f"❌ Error Type: {error_type}")
                print(f"   Message: {error_msg}")
        else:
            print(f"❌ HTTP {response.status_code}: {response.text}")
    
    except Exception as e:
        print(f"❌ Connection Error: {e}")
    
    print("\n🔗 Useful Links:")
    print("   • Billing Dashboard: https://platform.openai.com/settings/organization/billing")
    print("   • Usage Dashboard: https://platform.openai.com/usage")
    print("   • API Keys: https://platform.openai.com/api-keys")
    print("   • Pricing: https://openai.com/api/pricing/")

def check_anthropic_account(api_key):
    """Check Anthropic API account status"""
    print("\n" + "="*80)
    print("ANTHROPIC API ACCOUNT STATUS")
    print("="*80)
    
    if not api_key:
        print("❌ No Anthropic API key configured")
        print("\n💡 Anthropic Claude API:")
        print("   • Sign up: https://console.anthropic.com/")
        print("   • Pricing: ~$0.003/1K input tokens (Claude Haiku)")
        print("   • Also pay-as-you-go, no fixed monthly fee")
        return
    
    print(f"API Key: {api_key[:20]}...{api_key[-5:]}")
    
    headers = {
        "x-api-key": api_key,
        "anthropic-version": "2023-06-01",
        "content-type": "application/json"
    }
    
    print("\n📊 Checking Account Status...")
    print("-" * 80)
    
    test_url = "https://api.anthropic.com/v1/messages"
    test_payload = {
        "model": "claude-3-5-sonnet-20241022",
        "max_tokens": 5,
        "messages": [{"role": "user", "content": "test"}]
    }
    
    try:
        response = requests.post(test_url, headers=headers, json=test_payload, timeout=10)
        
        if response.status_code == 200:
            print("✅ API Key is ACTIVE and has available credits")
        elif response.status_code == 401:
            print("❌ API Key is INVALID")
        elif response.status_code == 429:
            print("⚠️ RATE LIMIT or QUOTA EXCEEDED")
            print(f"   Response: {response.text}")
        else:
            print(f"❌ HTTP {response.status_code}")
            print(f"   Response: {response.text}")
    
    except Exception as e:
        print(f"❌ Connection Error: {e}")
    
    print("\n🔗 Useful Links:")
    print("   • Console: https://console.anthropic.com/")
    print("   • Pricing: https://www.anthropic.com/pricing")

def main():
    print("="*80)
    print(" API ACCOUNT STATUS CHECKER")
    print("="*80)
    print("\n⚠️  IMPORTANT: ChatGPT Plus ≠ OpenAI API")
    print("   ChatGPT Plus ($20/month) is for web use only")
    print("   OpenAI API requires separate credits (pay-per-use)")
    
    config = read_config()
    
    check_openai_account(config['openai'])
    check_anthropic_account(config['anthropic'])
    
    print("\n" + "="*80)
    print(" RECOMMENDATIONS")
    print("="*80)
    print("\n💰 For Fixed Monthly Fee:")
    print("   Unfortunately, major AI APIs (OpenAI, Anthropic) are pay-as-you-go")
    print("   No fixed monthly fee options for API access")
    
    print("\n💡 Cost Control Options:")
    print("   1. Set usage limits in OpenAI billing settings")
    print("   2. Use cheaper models (GPT-3.5 vs GPT-4)")
    print("   3. Monitor usage dashboard regularly")
    print("   4. Start with small credit amount ($5-10)")
    
    print("\n📊 Typical Costs for Light Usage:")
    print("   • 100 messages/month with GPT-3.5: ~$0.50")
    print("   • 100 messages/month with GPT-4: ~$3-5")
    print("   • Much cheaper than $20/month ChatGPT Plus")

if __name__ == '__main__':
    main()
