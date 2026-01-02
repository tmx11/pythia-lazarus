#!/usr/bin/env python3
"""
Test script for Pythia API connections
Reads the same pythia.ini config file and tests OpenAI/Anthropic APIs
"""

import os
import sys
import json
import configparser
import requests
from pathlib import Path

def get_config_path():
    """Get the same config path that Delphi code uses"""
    appdata = os.environ.get('APPDATA')
    if not appdata:
        print("ERROR: APPDATA environment variable not found")
        sys.exit(1)
    
    config_dir = os.path.join(appdata, 'Pythia')
    config_file = os.path.join(config_dir, 'pythia.ini')
    return config_file

def read_config():
    """Read API keys from pythia.ini"""
    config_file = get_config_path()
    
    if not os.path.exists(config_file):
        print(f"ERROR: Config file not found: {config_file}")
        sys.exit(1)
    
    print(f"Reading config from: {config_file}")
    
    config = configparser.ConfigParser()
    config.read(config_file)
    
    openai_key = config.get('API', 'OpenAIKey', fallback='')
    anthropic_key = config.get('API', 'AnthropicKey', fallback='')
    
    return {
        'openai': openai_key,
        'anthropic': anthropic_key
    }

def test_openai_connection(api_key):
    """Test OpenAI API with minimal request"""
    print("\n" + "="*70)
    print("TESTING OPENAI API CONNECTION")
    print("="*70)
    
    if not api_key:
        print("ERROR: No OpenAI API key configured")
        return False
    
    print(f"API Key: {api_key[:20]}...{api_key[-5:]} (length: {len(api_key)})")
    
    endpoint = "https://api.openai.com/v1/chat/completions"
    print(f"Endpoint: {endpoint}")
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    
    payload = {
        "model": "gpt-3.5-turbo",
        "messages": [
            {"role": "user", "content": "Say 'Connection successful' if you receive this."}
        ],
        "max_tokens": 50
    }
    
    print(f"\nRequest payload:")
    print(json.dumps(payload, indent=2))
    print(f"\nSending POST request...")
    
    try:
        response = requests.post(endpoint, headers=headers, json=payload, timeout=30)
        
        print(f"\nHTTP Status Code: {response.status_code}")
        print(f"Response Headers:")
        for key, value in response.headers.items():
            if key.lower() in ['content-type', 'x-ratelimit-limit-requests', 'x-ratelimit-remaining-requests', 'x-ratelimit-reset-requests']:
                print(f"  {key}: {value}")
        
        print(f"\nResponse Body:")
        try:
            response_json = response.json()
            print(json.dumps(response_json, indent=2))
            
            if response.status_code == 200:
                message = response_json['choices'][0]['message']['content']
                print(f"\n✓ SUCCESS! AI Response: {message}")
                return True
            else:
                print(f"\n✗ FAILED! Error response from API")
                return False
                
        except json.JSONDecodeError:
            print(response.text)
            return False
            
    except requests.exceptions.RequestException as e:
        print(f"\n✗ REQUEST FAILED!")
        print(f"Error: {e}")
        return False

def test_anthropic_connection(api_key):
    """Test Anthropic API with minimal request"""
    print("\n" + "="*70)
    print("TESTING ANTHROPIC API CONNECTION")
    print("="*70)
    
    if not api_key:
        print("ERROR: No Anthropic API key configured")
        return False
    
    print(f"API Key: {api_key[:20]}...{api_key[-5:]} (length: {len(api_key)})")
    
    endpoint = "https://api.anthropic.com/v1/messages"
    print(f"Endpoint: {endpoint}")
    
    headers = {
        "Content-Type": "application/json",
        "x-api-key": api_key,
        "anthropic-version": "2023-06-01"
    }
    
    payload = {
        "model": "claude-3-5-sonnet-20241022",
        "max_tokens": 50,
        "messages": [
            {"role": "user", "content": "Say 'Connection successful' if you receive this."}
        ]
    }
    
    print(f"\nRequest payload:")
    print(json.dumps(payload, indent=2))
    print(f"\nSending POST request...")
    
    try:
        response = requests.post(endpoint, headers=headers, json=payload, timeout=30)
        
        print(f"\nHTTP Status Code: {response.status_code}")
        print(f"\nResponse Body:")
        try:
            response_json = response.json()
            print(json.dumps(response_json, indent=2))
            
            if response.status_code == 200:
                message = response_json['content'][0]['text']
                print(f"\n✓ SUCCESS! AI Response: {message}")
                return True
            else:
                print(f"\n✗ FAILED! Error response from API")
                return False
                
        except json.JSONDecodeError:
            print(response.text)
            return False
            
    except requests.exceptions.RequestException as e:
        print(f"\n✗ REQUEST FAILED!")
        print(f"Error: {e}")
        return False

def main():
    print("Pythia API Connection Test Tool")
    print("="*70)
    
    # Read config
    config = read_config()
    
    # Test connections
    openai_result = False
    anthropic_result = False
    
    if config['openai']:
        openai_result = test_openai_connection(config['openai'])
    else:
        print("\nSkipping OpenAI test - no API key configured")
    
    if config['anthropic']:
        anthropic_result = test_anthropic_connection(config['anthropic'])
    else:
        print("\nSkipping Anthropic test - no API key configured")
    
    # Summary
    print("\n" + "="*70)
    print("SUMMARY")
    print("="*70)
    print(f"OpenAI: {'✓ Connected' if openai_result else '✗ Failed or not configured'}")
    print(f"Anthropic: {'✓ Connected' if anthropic_result else '✗ Failed or not configured'}")
    
    if not (openai_result or anthropic_result):
        print("\nNOTE: REST APIs are stateless - no persistent connection exists.")
        print("Each request is independent with authentication in headers.")
        sys.exit(1)
    else:
        print("\n✓ At least one API connection is working!")
        sys.exit(0)

if __name__ == '__main__':
    main()
