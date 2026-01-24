# linkedin-crosspost

Generate LinkedIn teasers from Ghost blog posts and send them to Zapier for posting.

## Installation

```bash
# Install Poetry if you haven't already
curl -sSL https://install.python-poetry.org | python3 -

# Install dependencies
cd linkedin-crosspost
poetry install

# Activate the virtual environment
poetry shell
```

## Configuration

1. Copy the example config:
   ```bash
   cp config.example.yaml config.yaml
   ```

2. Fill in your values:
   - **Ghost API URL**: Your Ghost site URL
   - **Ghost API Key**: From Ghost Admin → Settings → Integrations → Add custom integration
   - **Zapier Webhook**: From your Zap (Webhooks by Zapier → Catch Hook)

## Usage

### List recent posts

```bash
linkedin-crosspost list
```

### Generate a teaser (preview)

```bash
# From latest post
linkedin-crosspost generate

# From specific post
linkedin-crosspost generate --slug my-post-slug

# Override strategy
linkedin-crosspost generate --strategy excerpt --max-chars 1500
```

### Send to Zapier (for LinkedIn posting)

```bash
# Dry run (preview without sending)
linkedin-crosspost post --dry-run

# Actually send
linkedin-crosspost post

# Specific post
linkedin-crosspost post --slug my-post-slug
```

## Teaser Strategies

| Strategy | Description |
|----------|-------------|
| `first_paragraphs` | First N paragraphs that fit under character limit (default) |
| `custom_marker` | Content before `<!--teaser-->` marker in your post |
| `excerpt` | Use Ghost's built-in excerpt/custom_excerpt field |

## Zapier Setup

1. Create a new Zap
2. Trigger: **Webhooks by Zapier** → **Catch Hook**
3. Copy the webhook URL to your `config.yaml`
4. Action: **LinkedIn** → **Create Share Update**
5. Map the fields:
   - Commentary: `{{content}}`
   - Link URL: `{{url}}` (optional)

## Example Output

Input (Ghost post):
```
Title: Why I'm Rethinking Technical Writing
Content: <p>For years, I've written technical content...</p>
```

Output (LinkedIn post):
```
Why I'm Rethinking Technical Writing

For years, I've written technical content the same way.
Documents full of details, comprehensive to a fault.

But recently, I realized something important...

→ Read more: https://ink08.net/rethinking-technical-writing/
```
