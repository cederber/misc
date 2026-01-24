"""LinkedIn cross-posting CLI for Ghost blogs."""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Any

import click
import requests
import yaml
from bs4 import BeautifulSoup


def load_config(config_path: Path) -> dict[str, Any]:
    """Load configuration from YAML file."""
    if not config_path.exists():
        raise click.ClickException(
            f"Config file not found: {config_path}\n"
            f"Copy config.example.yaml to config.yaml and fill in your values."
        )
    with open(config_path) as f:
        return yaml.safe_load(f)


def fetch_posts(
    api_url: str,
    api_key: str,
    limit: int = 10,
    include_html: bool = True,
) -> list[dict[str, Any]]:
    """Fetch recent posts from Ghost Content API."""
    url = f"{api_url.rstrip('/')}/ghost/api/content/posts/"
    params = {
        "key": api_key,
        "limit": limit,
        "order": "published_at desc",
        "filter": "visibility:public",
    }
    if include_html:
        params["formats"] = "html"

    response = requests.get(url, params=params, timeout=30)
    response.raise_for_status()

    data = response.json()
    return data.get("posts", [])


def fetch_post_by_slug(api_url: str, api_key: str, slug: str) -> dict[str, Any] | None:
    """Fetch a specific post by its slug."""
    url = f"{api_url.rstrip('/')}/ghost/api/content/posts/slug/{slug}/"
    params = {
        "key": api_key,
        "formats": "html",
    }

    response = requests.get(url, params=params, timeout=30)
    if response.status_code == 404:
        return None
    response.raise_for_status()

    data = response.json()
    posts = data.get("posts", [])
    return posts[0] if posts else None


def html_to_text(html: str) -> str:
    """Convert HTML to plain text, preserving paragraph breaks."""
    soup = BeautifulSoup(html, "html.parser")

    # Remove script and style elements
    for element in soup(["script", "style"]):
        element.decompose()

    # Replace block elements with newlines
    for tag in soup.find_all(["p", "br", "h1", "h2", "h3", "h4", "h5", "h6", "li"]):
        tag.insert_after("\n\n")

    text = soup.get_text()

    # Clean up whitespace
    lines = [line.strip() for line in text.splitlines()]
    text = "\n".join(lines)

    # Collapse multiple newlines to double
    text = re.sub(r"\n{3,}", "\n\n", text)

    return text.strip()


def extract_teaser_first_paragraphs(html: str, max_chars: int = 2800) -> str:
    """Extract teaser using first N paragraphs that fit under char limit."""
    soup = BeautifulSoup(html, "html.parser")

    paragraphs = []
    current_length = 0

    for p in soup.find_all(["p", "h2", "h3"]):
        text = p.get_text().strip()
        if not text:
            continue

        # Check if adding this paragraph exceeds limit
        new_length = current_length + len(text) + 2  # +2 for newlines
        if new_length > max_chars and paragraphs:
            break

        paragraphs.append(text)
        current_length = new_length

    return "\n\n".join(paragraphs)


def extract_teaser_custom_marker(html: str, marker: str = "<!--teaser-->") -> str:
    """Extract teaser using custom HTML comment marker."""
    if marker in html:
        teaser_html = html.split(marker)[0]
        return html_to_text(teaser_html)
    # Fall back to first paragraphs if marker not found
    return extract_teaser_first_paragraphs(html)


def extract_teaser_excerpt(post: dict[str, Any], max_chars: int = 2800) -> str:
    """Use Ghost's built-in excerpt field."""
    excerpt = post.get("excerpt", "") or post.get("custom_excerpt", "")
    if excerpt:
        return excerpt[:max_chars]
    # Fall back to first paragraphs
    return extract_teaser_first_paragraphs(post.get("html", ""), max_chars)


def extract_teaser(
    post: dict[str, Any],
    strategy: str = "first_paragraphs",
    max_chars: int = 2800,
    marker: str = "<!--teaser-->",
) -> str:
    """Extract teaser from post using specified strategy."""
    html = post.get("html", "")

    if strategy == "first_paragraphs":
        return extract_teaser_first_paragraphs(html, max_chars)
    elif strategy == "custom_marker":
        return extract_teaser_custom_marker(html, marker)
    elif strategy == "excerpt":
        return extract_teaser_excerpt(post, max_chars)
    else:
        raise ValueError(f"Unknown teaser strategy: {strategy}")


def format_for_linkedin(
    teaser: str,
    post_title: str,
    post_url: str,
    include_title: bool = True,
    cta_template: str = "\n\n→ Read more: {url}",
    max_total_chars: int = 3000,
) -> str:
    """Format teaser for LinkedIn posting."""
    parts = []

    if include_title:
        parts.append(post_title)
        parts.append("")  # Blank line after title

    parts.append(teaser)

    cta = cta_template.format(url=post_url, title=post_title)
    parts.append(cta)

    result = "\n".join(parts)

    # Truncate if too long (shouldn't happen if teaser extraction is configured right)
    if len(result) > max_total_chars:
        # Leave room for CTA
        available = max_total_chars - len(cta) - 10
        result = result[:available] + "..." + cta

    return result


def send_to_zapier(webhook_url: str, content: str, post_url: str) -> dict[str, Any]:
    """Send formatted content to Zapier webhook."""
    payload = {
        "content": content,
        "url": post_url,
    }

    response = requests.post(webhook_url, json=payload, timeout=30)
    response.raise_for_status()

    return {"status": "sent", "response": response.text}


# --- CLI ---


@click.group()
@click.option(
    "--config",
    "-c",
    type=click.Path(exists=False, path_type=Path),
    default="config.yaml",
    help="Path to config file",
)
@click.pass_context
def cli(ctx: click.Context, config: Path) -> None:
    """LinkedIn cross-posting tool for Ghost blogs."""
    ctx.ensure_object(dict)
    ctx.obj["config_path"] = config


@cli.command()
@click.pass_context
def list(ctx: click.Context) -> None:
    """List recent posts from your Ghost blog."""
    config = load_config(ctx.obj["config_path"])
    ghost = config["ghost"]

    posts = fetch_posts(ghost["api_url"], ghost["api_key"], limit=10, include_html=False)

    if not posts:
        click.echo("No posts found.")
        return

    click.echo(f"{'Slug':<40} {'Title':<50} {'Published'}")
    click.echo("-" * 110)

    for post in posts:
        slug = post.get("slug", "")[:38]
        title = post.get("title", "")[:48]
        published = post.get("published_at", "")[:10]
        click.echo(f"{slug:<40} {title:<50} {published}")


@cli.command()
@click.option("--slug", "-s", help="Post slug (default: latest post)")
@click.option("--strategy", type=click.Choice(["first_paragraphs", "custom_marker", "excerpt"]))
@click.option("--max-chars", type=int, help="Maximum teaser characters")
@click.pass_context
def generate(
    ctx: click.Context,
    slug: str | None,
    strategy: str | None,
    max_chars: int | None,
) -> None:
    """Generate LinkedIn teaser from a Ghost post."""
    config = load_config(ctx.obj["config_path"])
    ghost = config["ghost"]
    teaser_config = config.get("teaser", {})

    # Get post
    if slug:
        post = fetch_post_by_slug(ghost["api_url"], ghost["api_key"], slug)
        if not post:
            raise click.ClickException(f"Post not found: {slug}")
    else:
        posts = fetch_posts(ghost["api_url"], ghost["api_key"], limit=1)
        if not posts:
            raise click.ClickException("No posts found")
        post = posts[0]

    # Extract teaser
    teaser = extract_teaser(
        post,
        strategy=strategy or teaser_config.get("strategy", "first_paragraphs"),
        max_chars=max_chars or teaser_config.get("max_chars", 2800),
        marker=teaser_config.get("marker", "<!--teaser-->"),
    )

    # Format for LinkedIn
    linkedin_config = config.get("linkedin", {})
    formatted = format_for_linkedin(
        teaser,
        post_title=post.get("title", ""),
        post_url=post.get("url", ""),
        include_title=linkedin_config.get("include_title", True),
        cta_template=linkedin_config.get("cta_template", "\n\n→ Read more: {url}"),
    )

    click.echo(formatted)
    click.echo("\n" + "=" * 60)
    click.echo(f"Characters: {len(formatted)}")


@cli.command()
@click.option("--slug", "-s", help="Post slug (default: latest post)")
@click.option("--dry-run", is_flag=True, help="Print instead of sending to Zapier")
@click.pass_context
def post(ctx: click.Context, slug: str | None, dry_run: bool) -> None:
    """Generate teaser and send to Zapier for LinkedIn posting."""
    config = load_config(ctx.obj["config_path"])
    ghost = config["ghost"]
    teaser_config = config.get("teaser", {})
    zapier_config = config.get("zapier", {})

    webhook_url = zapier_config.get("webhook_url")
    if not webhook_url and not dry_run:
        raise click.ClickException("Zapier webhook_url not configured")

    # Get post
    if slug:
        post_data = fetch_post_by_slug(ghost["api_url"], ghost["api_key"], slug)
        if not post_data:
            raise click.ClickException(f"Post not found: {slug}")
    else:
        posts = fetch_posts(ghost["api_url"], ghost["api_key"], limit=1)
        if not posts:
            raise click.ClickException("No posts found")
        post_data = posts[0]

    # Extract teaser
    teaser = extract_teaser(
        post_data,
        strategy=teaser_config.get("strategy", "first_paragraphs"),
        max_chars=teaser_config.get("max_chars", 2800),
        marker=teaser_config.get("marker", "<!--teaser-->"),
    )

    # Format for LinkedIn
    linkedin_config = config.get("linkedin", {})
    formatted = format_for_linkedin(
        teaser,
        post_title=post_data.get("title", ""),
        post_url=post_data.get("url", ""),
        include_title=linkedin_config.get("include_title", True),
        cta_template=linkedin_config.get("cta_template", "\n\n→ Read more: {url}"),
    )

    if dry_run:
        click.echo("=== DRY RUN (would send to Zapier) ===\n")
        click.echo(formatted)
        click.echo(f"\n=== Characters: {len(formatted)} ===")
        return

    # Send to Zapier
    result = send_to_zapier(webhook_url, formatted, post_data.get("url", ""))
    click.echo(f"Sent to Zapier: {result['status']}")
    click.echo(f"Post: {post_data.get('title')}")


if __name__ == "__main__":
    cli()
