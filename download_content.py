#!/usr/bin/env python3
"""Recursively download all files from the CS252 labs site.

Behavior:
- Crawls links recursively under the provided base URL.
- Downloads files into a local content directory.
- Flattens output so only files exist in content/.
- Encodes each file's relative path in the local filename for easier context use.
"""

from __future__ import annotations

import argparse
import re
import shutil
import sys
from html.parser import HTMLParser
from pathlib import Path
from typing import Iterable
from urllib.parse import urljoin, urlparse, unquote
from urllib.request import Request, urlopen


BASE_URL = "https://www.cs.sjsu.edu/~austin/cs252-spring26/labs/"
DEFAULT_OUTPUT_DIR = "content"
USER_AGENT = "Mozilla/5.0 (compatible; cs252-downloader/1.0)"


class LinkExtractor(HTMLParser):
	"""Minimal HTML link extractor for href values."""

	def __init__(self) -> None:
		super().__init__()
		self.links: list[str] = []

	def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
		if tag.lower() != "a":
			return
		for key, value in attrs:
			if key.lower() == "href" and value:
				self.links.append(value)


def normalize_directory_url(url: str) -> str:
	"""Ensure directory URLs end with '/' for stable joining and prefix checks."""
	return url if url.endswith("/") else f"{url}/"


def strip_url_noise(url: str) -> str:
	"""Remove query/fragment so one logical resource maps to one URL key."""
	parsed = urlparse(url)
	return parsed._replace(query="", fragment="").geturl()


def fetch_bytes(url: str) -> bytes:
	req = Request(url, headers={"User-Agent": USER_AGENT})
	with urlopen(req, timeout=30) as response:
		return response.read()


def fetch_resource(url: str) -> tuple[bytes, str, str]:
	"""Fetch URL and return (data, content_type, final_url_after_redirects)."""
	req = Request(url, headers={"User-Agent": USER_AGENT})
	with urlopen(req, timeout=30) as response:
		data = response.read()
		content_type = response.headers.get("Content-Type", "")
		final_url = response.geturl()
	return data, content_type, final_url


def fetch_html(url: str) -> str:
	data = fetch_bytes(url)
	return data.decode("utf-8", errors="replace")


def is_within_base(candidate_url: str, base_url: str) -> bool:
	"""Only keep links that stay on the same host and below the labs path."""
	parsed_candidate = urlparse(candidate_url)
	parsed_base = urlparse(base_url)
	if parsed_candidate.netloc != parsed_base.netloc:
		return False
	base_path = normalize_directory_url(parsed_base.path)
	return parsed_candidate.path.startswith(base_path)


def clean_relative_path(url: str, base_url: str) -> str:
	"""Return path under base URL, without query/fragment."""
	candidate_path = urlparse(url).path
	base_path = normalize_directory_url(urlparse(base_url).path)
	rel = candidate_path[len(base_path) :]
	rel = unquote(rel)
	rel = rel.lstrip("/")
	return rel


def sanitize_component(name: str) -> str:
	"""Sanitize a path component for broad filesystem compatibility."""
	cleaned = re.sub(r"[\\/:*?\"<>|]", "_", name)
	return cleaned.strip() or "_"


def build_output_file_path(relative_path: str, output_root: Path) -> Path:
	"""Build a flat output path using a path-encoded filename.

	Example:
		remote: lab1/starter/code.hs
		local:  content/lab1__starter__code.hs
	"""
	raw = relative_path.strip()
	if not raw:
		raw = "index.html"
	elif raw.endswith("/"):
		raw = f"{raw}index.html"

	rel = raw.strip("/")

	remote_parts = [p for p in rel.split("/") if p]
	safe_parts = [sanitize_component(p) for p in remote_parts]

	file_name = safe_parts[-1]

	path_encoded_name = "__".join(safe_parts)
	if "." in file_name:
		ext = file_name.rsplit(".", 1)[1]
		if not path_encoded_name.lower().endswith(f".{ext.lower()}"):
			path_encoded_name = f"{path_encoded_name}.{ext}"

	return output_root / path_encoded_name


def extract_links_from_html(html: str, page_url: str) -> Iterable[str]:
	parser = LinkExtractor()
	parser.feed(html)
	for href in parser.links:
		yield urljoin(page_url, href)


def looks_like_html(data: bytes, content_type: str, url: str) -> bool:
	ct = content_type.lower()
	if "text/html" in ct or "application/xhtml+xml" in ct:
		return True

	path = urlparse(url).path.lower()
	if path.endswith(".html") or path.endswith(".htm"):
		return True

	prefix = data[:256].lstrip().lower()
	return prefix.startswith(b"<!doctype html") or prefix.startswith(b"<html")


def crawl_and_download(base_url: str, output_dir: Path) -> tuple[int, int]:
	"""Recursively crawl pages and download files.

	Returns:
	  (num_urls_visited, num_files_downloaded)
	"""
	base_url = normalize_directory_url(base_url)
	output_dir.mkdir(parents=True, exist_ok=True)

	visited_urls: set[str] = set()
	queued_urls: list[str] = [base_url]
	downloaded_files: set[str] = set()

	while queued_urls:
		current_url = strip_url_noise(queued_urls.pop())
		if current_url in visited_urls:
			continue

		print(f"[url ] {current_url}")
		visited_urls.add(current_url)

		try:
			data, content_type, final_url = fetch_resource(current_url)
		except Exception as exc:  # noqa: BLE001
			print(f"[warn] Could not fetch {current_url}: {exc}")
			continue

		canonical_url = strip_url_noise(final_url)
		if canonical_url not in visited_urls:
			visited_urls.add(canonical_url)

		relative_path = clean_relative_path(canonical_url, base_url)
		local_path = build_output_file_path(relative_path, output_dir)
		local_path.parent.mkdir(parents=True, exist_ok=True)

		try:
			local_path.write_bytes(data)
			downloaded_files.add(relative_path or "index.html")
			print(f"[file] {relative_path or 'index.html'} -> {local_path}")
		except Exception as exc:  # noqa: BLE001
			print(f"[warn] Could not write {local_path}: {exc}")
			continue

		if not looks_like_html(data, content_type, canonical_url):
			continue

		html = data.decode("utf-8", errors="replace")
		links = list(extract_links_from_html(html, canonical_url))

		for link in links:
			link = strip_url_noise(link)
			if not is_within_base(link, base_url):
				continue

			if link not in visited_urls:
				queued_urls.append(link)

	return len(visited_urls), len(downloaded_files)


def parse_args() -> argparse.Namespace:
	parser = argparse.ArgumentParser(
		description="Recursively download all files from a labs directory listing."
	)
	parser.add_argument(
		"--url",
		default=BASE_URL,
		help=f"Base labs URL to crawl (default: {BASE_URL})",
	)
	parser.add_argument(
		"--out",
		default=DEFAULT_OUTPUT_DIR,
		help=f"Output directory for downloaded files (default: {DEFAULT_OUTPUT_DIR})",
	)
	return parser.parse_args()


def main() -> int:
	args = parse_args()
	base_url = normalize_directory_url(args.url)
	out_dir = Path(args.out)

	print(f"Starting crawl: {base_url}")
	print(f"Saving content under: {out_dir.resolve()}")
	if out_dir.exists():
		print(f"Clearing existing output directory: {out_dir.resolve()}")
		shutil.rmtree(out_dir)

	num_dirs, num_files = crawl_and_download(base_url, out_dir)

	print("Done.")
	print(f"Directories crawled: {num_dirs}")
	print(f"Files downloaded: {num_files}")
	return 0


if __name__ == "__main__":
	sys.exit(main())
