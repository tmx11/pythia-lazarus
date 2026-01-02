#!/usr/bin/env python3
"""
Test GitHub Models API - FREE tier with same models as Copilot
https://github.com/marketplace/models
"""

import os
import json
import requests

def test_github_models():
    print("="*80)
    print("GITHUB MODELS API TEST (FREE TIER)")
    print("="*80)
    
    print("\n📋 About GitHub Models:")
    print("   • FREE API using same models as Copilot")
    print("   • Claude Sonnet 3.5, GPT-4, Llama, etc.")
    print("   • Authenticate with GitHub Personal Access Token")
    print("   • Rate limited but sufficient for development")
    
    print("\n🔑 How to get GitHub Token:")
    print("   1. Go to: https://github.com/settings/tokens")
    print("   2. Click 'Generate new token' > 'Generate new token (classic)'")
    print("   3. Select scopes: 'repo' (or just 'public_repo')")
    print("   4. Click 'Generate token'")
    print("   5. Copy the token (starts with 'ghp_')")
    
    # Check if token exists in environment
    token = os.environ.get('GITHUB_TOKEN') or os.environ.get('GH_TOKEN')
    
    if not token:
        print("\n⚠️ No GitHub token found in environment")
        print("   Set environment variable: GITHUB_TOKEN=your_token")
        print("\n   Or run: setx GITHUB_TOKEN your_token_here")
        return False
    
    print(f"\n✅ GitHub token found: {token[:7]}...")
    
    # Test Claude Sonnet 3.5 via GitHub Models
    endpoint = "https://models.inference.ai.azure.com/chat/completions"
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {token}"
    }
    
    payload = {
        "model": "claude-3-5-sonnet",  # Available models on GitHub Models
        "messages": [
            {
                "role": "system",
                "content": "You are a helpful Delphi programming assistant."
            },
            {
                "role": "user",
                "content": "Say 'GitHub Models API is working!' if you receive this."
            }
        ],
        "max_tokens": 100,
        "temperature": 0.7
    }
    
    print("\n🔄 Testing connection...")
    print(f"Endpoint: {endpoint}")
    print(f"Model: Claude 3.5 Sonnet")
    
    try:
        response = requests.post(endpoint, headers=headers, json=payload, timeout=30)
        
        print(f"\nHTTP Status: {response.status_code}")
        
        if response.status_code == 200:
            data = response.json()
            message = data['choices'][0]['message']['content']
            print(f"\n✅ SUCCESS!")
            print(f"AI Response: {message}")
            
            print("\n💰 Cost: FREE (rate limited)")
            print("🎯 This is EXACTLY what you want for Pythia!")
            return True
        else:
            print(f"\n❌ Error Response:")
            print(response.text)
            return False
            
    except Exception as e:
        print(f"\n❌ Connection failed: {e}")
        return False

def show_available_models():
    print("\n" + "="*80)
    print("AVAILABLE MODELS ON GITHUB (FREE)")
    print("="*80)
    
    models = [
        ("Claude 3.5 Sonnet", "claude-3-5-sonnet", "Anthropic's latest, same as Copilot"),
        ("GPT-4o", "gpt-4o", "OpenAI's latest GPT-4"),
        ("GPT-4o mini", "gpt-4o-mini", "Faster, cheaper GPT-4"),
        ("Llama 3.3 70B", "llama-3.3-70b", "Meta's open source model"),
        ("Phi-4", "phi-4", "Microsoft's small model")
    ]
    
    for name, model_id, desc in models:
        print(f"\n• {name}")
        print(f"  ID: {model_id}")
        print(f"  {desc}")

def main():
    result = test_github_models()
    show_available_models()
    
    print("\n" + "="*80)
    print("NEXT STEPS FOR PYTHIA")
    print("="*80)
    
    if result:
        print("\n✅ GitHub Models API is working!")
        print("\n📝 To integrate with Pythia:")
        print("   1. Add GitHub token to Pythia settings")
        print("   2. Modify Pythia.AI.Client.pas to use GitHub Models endpoint")
        print("   3. No more OpenAI/Anthropic billing needed!")
        print("   4. Same models, FREE tier, no credit card!")
    else:
        print("\n⚠️ Setup needed:")
        print("   1. Create GitHub Personal Access Token")
        print("   2. Set GITHUB_TOKEN environment variable")
        print("   3. Run this test again")
    
    print("\n📚 Documentation:")
    print("   • GitHub Models: https://github.com/marketplace/models")
    print("   • API Docs: https://docs.github.com/en/github-models")
    print("   • Model Catalog: https://github.com/marketplace/models")

if __name__ == '__main__':
    main()
