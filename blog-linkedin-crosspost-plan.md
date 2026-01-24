# Blog + LinkedIn Cross-Posting Implementation Plan

## Overview

This plan outlines setting up a personal blog at **ink08.net** with:
- Ghost Pro for blogging and newsletters
- Mailgun for email delivery
- Python tooling for content transformation
- Zapier for LinkedIn cross-posting
- ActivityPub for fediverse reach

---

## Goals

1. Substack-like experience: persistent blog post + email newsletter with matching formatting
2. Full ownership and control ("open web" feel, not locked into proprietary platform)
3. No paid subscription/paywall features needed
4. Algorithmic-feed reach via LinkedIn, with content living on own domain
5. Cross-posts as teaser + link, with flexibility for future customization
6. Zapier for LinkedIn posting (no custom API code)
7. Open source blogging platform

---

## Architecture

```
                                    YOU
                                     │
                                     ▼
                            ┌────────────────┐
                            │ Write post in  │
                            │ Ghost Admin    │
                            └────────────────┘
                                     │
                                     ▼ [Publish]
              ┌──────────────────────┼──────────────────────┐
              │                      │                      │
              ▼                      ▼                      ▼
    ┌──────────────────┐   ┌──────────────────┐   ┌──────────────────┐
    │ Live on          │   │ Email sent to    │   │ Appears in       │
    │ ink08.net        │   │ subscribers via  │   │ Fediverse feeds  │
    │                  │   │ Mailgun          │   │ (ActivityPub)    │
    └──────────────────┘   └──────────────────┘   └──────────────────┘
              │
              │ [Run Python script or auto-webhook]
              ▼
    ┌──────────────────┐
    │ Python teaser    │
    │ generator        │
    └──────────────────┘
              │
              ▼
    ┌──────────────────┐
    │ Zapier webhook   │
    └──────────────────┘
              │
              ▼
    ┌──────────────────┐
    │ LinkedIn post    │
    │ with teaser +    │
    │ link             │
    └──────────────────┘
```

---

## Phase 1: Ghost Pro Setup

### 1.1 Sign up for Ghost Pro
- Go to [ghost.org/pricing](https://ghost.org/pricing/)
- Choose **Starter** ($9/mo) — sufficient for starting out
  - 500 members included
  - Can upgrade to Creator ($25/mo) later if needed

### 1.2 Configure publication
- Set publication name, description, logo
- Choose a theme (default Casper is clean, or browse [Ghost Themes](https://ghost.org/themes/))
- Configure navigation menu

---

## Phase 2: Domain Configuration

### 2.1 Point ink08.net to Ghost Pro

Ghost Pro provides a subdomain like `yoursite.ghost.io`. To use `ink08.net`:

1. In Ghost Admin → Settings → General → Publication URL, set `https://ink08.net`
2. Add DNS records at your registrar:

| Type  | Name | Value                           |
|-------|------|---------------------------------|
| A     | @    | (Ghost's IP - provided in admin)|
| CNAME | www  | yoursite.ghost.io               |

3. Ghost Pro handles SSL certificate automatically

### 2.2 Email subdomain for newsletters

Configure for Mailgun sending:

| Type  | Name                | Value                    |
|-------|---------------------|--------------------------|
| TXT   | @                   | (Mailgun SPF record)     |
| TXT   | mailgun._domainkey  | (Mailgun DKIM record)    |
| CNAME | email               | mailgun.org              |

---

## Phase 3: Newsletter Setup (Mailgun)

### 3.1 Create Mailgun account
- Sign up at [mailgun.com](https://www.mailgun.com/)
- Free tier: 1,000 emails/month for 3 months
- Then ~$0.80/1,000 emails (very affordable for personal use)

### 3.2 Add domain to Mailgun
- In Mailgun dashboard → Sending → Domains → Add New Domain
- Use subdomain: `mail.ink08.net` or `newsletter.ink08.net`
- Add the DNS records Mailgun provides (SPF, DKIM, tracking)

### 3.3 Connect Mailgun to Ghost
- In Ghost Admin → Settings → Email newsletter
- Enter Mailgun API key and domain
- Send a test email to verify

### 3.4 Configure newsletter settings
- Sender name: Your name
- Sender email: `newsletter@ink08.net`
- Enable "Send newsletter on publish" option per-post

---

## Phase 4: Python Teaser Generator

Custom code to transform Ghost posts into LinkedIn-ready teasers.

### 4.1 Architecture

**Recommended: Start with local CLI script**

| Option              | Pros                        | Cons                        |
|---------------------|-----------------------------|-----------------------------|
| Local CLI script    | Simple, no hosting cost     | Manual trigger required     |
| Cloud Function      | Auto-triggers on webhook    | Slight complexity           |
| Always-on server    | Most flexible               | Overkill for this use case  |

### 4.2 Core Functionality

```python
# teaser_generator.py - conceptual outline

def fetch_latest_post(ghost_api_url: str, api_key: str) -> dict:
    """Fetch the most recent published post from Ghost API."""
    # GET /ghost/api/content/posts/?limit=1&order=published_at%20desc
    pass

def extract_teaser(post_html: str, strategy: str = "first_paragraphs",
                   max_chars: int = 2800) -> str:
    """
    Extract teaser from post content.

    Strategies:
    - "first_paragraphs": First N paragraphs under char limit
    - "custom_marker": Content before <!--teaser--> marker
    - "excerpt": Use Ghost's built-in excerpt field
    """
    pass

def format_for_linkedin(teaser: str, post_url: str, post_title: str) -> str:
    """
    Format teaser for LinkedIn posting.

    - Strip HTML, preserve line breaks
    - Add post title as header
    - Append "Read more: {url}"
    - Ensure under 3000 char limit
    """
    pass

def send_to_zapier(formatted_content: str, zapier_webhook_url: str) -> None:
    """POST the formatted content to Zapier webhook."""
    pass
```

### 4.3 Ghost API Setup

1. In Ghost Admin → Settings → Integrations → Add custom integration
2. Name it "LinkedIn Cross-poster"
3. Copy the **Content API Key** and **API URL**

### 4.4 Example Transformation

**Input (Ghost post):**
```
Title: "Why I'm Rethinking My Approach to Technical Writing"
Content: "<p>For years, I've written technical content the same way...</p>
          <p>But recently, I realized something important...</p>
          <p>... [2000 more words] ...</p>"
```

**Output (LinkedIn post):**
```
Why I'm Rethinking My Approach to Technical Writing

For years, I've written technical content the same way. Documents full of
details, comprehensive to a fault.

But recently, I realized something important...

→ Read the full post: https://ink08.net/rethinking-technical-writing/
```

### 4.5 Configuration for Flexibility

```yaml
# config.yaml
teaser_defaults:
  strategy: "first_paragraphs"
  max_chars: 2800
  include_title: true
  cta_template: "→ Read more: {url}"

# Per-post overrides via Ghost post tags or custom fields
# e.g., tag "linkedin:full" could trigger a different strategy
```

---

## Phase 5: Zapier Integration

### 5.1 Pipeline Options

**Option A: Manual trigger (simpler)**
```
Python CLI → Zapier Webhook → LinkedIn
```

**Option B: Fully automated**
```
Ghost webhook → Cloud Function (Python) → Zapier → LinkedIn
```

### 5.2 Create Zapier Zap

1. **Trigger**: Webhooks by Zapier → Catch Hook
   - Creates URL: `https://hooks.zapier.com/hooks/catch/123/abc/`
   - Python script POSTs to this URL

2. **Action**: LinkedIn → Create Share Update
   - Connect LinkedIn account
   - Map webhook data:
     - Commentary: `{{content}}`
     - Link: `{{url}}` (optional)

### 5.3 Zapier Plan

| Plan    | Tasks/month | Cost    | Notes                        |
|---------|-------------|---------|------------------------------|
| Free    | 100         | $0      | Fine for <25 posts/month     |
| Starter | 750         | $20/mo  | Unnecessary for personal use |

---

## Phase 6: ActivityPub (Optional)

Ghost is building ActivityPub support (currently in public beta, shipping in Ghost 6.0).

### Benefits
- Posts appear in Mastodon/Threads/Flipboard timelines
- Discoverable as `@you@ink08.net` from any fediverse app
- Likes and replies flow back to Ghost dashboard

### Setup
- Enable in Ghost Admin when available
- Configure your fediverse handle

---

## Implementation Checklist

| Step | Task                                   | Est. Time        |
|------|----------------------------------------|------------------|
| 1    | Sign up Ghost Pro, basic config        | 30 min           |
| 2    | Configure ink08.net DNS                | 15 min + propagation |
| 3    | Set up Mailgun, connect to Ghost       | 45 min           |
| 4    | Send test newsletter                   | 10 min           |
| 5    | Create Ghost API integration           | 5 min            |
| 6    | Write Python teaser generator v1       | 2-3 hours        |
| 7    | Set up Zapier webhook + LinkedIn       | 30 min           |
| 8    | End-to-end test                        | 30 min           |
| 9    | Enable ActivityPub beta (optional)     | 15 min           |

---

## Monthly Costs

| Service             | Cost    |
|---------------------|---------|
| Ghost Pro Starter   | $9      |
| Mailgun (low volume)| ~$0-1   |
| Zapier Free         | $0      |
| Domain (owned)      | $0      |
| **Total**           | **~$10/month** |

---

## Why Ghost Pro?

### Company Background
- **Ghost Foundation**: UK-registered non-profit (company #8540663)
- **Zero outside investors**: Self-funded via Ghost Pro revenue (~$7.5M/year)
- **No owners**: Founders don't own any part; cannot be bought or sold
- **Open source**: 100% MIT licensed, always will be
- **Transparent**: Publishes live financial data publicly

### Content Rights
- You retain full ownership of your content
- Full export available (posts as JSON, members as CSV)
- Can self-host anytime with same data

### Open Web Alignment
- ActivityPub federation (beta now, core in Ghost 6.0)
- Not locked into proprietary ecosystem
- Your domain, your content, portable subscribers

---

## Next Steps

1. Sign up for Ghost Pro at [ghost.org](https://ghost.org/)
2. Begin DNS configuration for ink08.net
3. Set up Mailgun account
4. Create the Python teaser generator (code in this repo)
