"""Redirect to the Music Assistant party plugin's current guest join URL.

The join code is random and expires, but the `party/url` API command always
returns a live one, or null when guest access is disabled.
"""

import json
import os
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

MA_URL = os.environ["MA_URL"]
PORT = int(os.environ["PORT"])
with open(os.path.join(os.environ["CREDENTIALS_DIRECTORY"], "token")) as f:
    TOKEN = f.read().strip()


def party_url():
    req = urllib.request.Request(
        f"{MA_URL}/api",
        data=json.dumps({"command": "party/url"}).encode(),
        headers={
            "Authorization": f"Bearer {TOKEN}",
            "Content-Type": "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=5) as res:
        return json.load(res)


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        try:
            url = party_url()
        except Exception as e:
            self.log_error("party/url failed: %r", e)
            self.respond(502, "Could not reach Music Assistant.\n")
            return
        if url:
            self.respond(302, "", {"Location": url})
        else:
            self.respond(404, "No party right now.\n")

    def respond(self, status, body, headers={}):
        data = body.encode()
        self.send_response(status)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        for key, value in headers.items():
            self.send_header(key, value)
        self.end_headers()
        self.wfile.write(data)


ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
