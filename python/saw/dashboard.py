import requests
from typing import Optional, Any, Type, Callable

MYXINE_PORT = 1123

# Loading SVG is licensed for free reuse from https://icons8.com/preloaders/
LOADING_SVG = \
    """<svg xmlns:svg="http://www.w3.org/2000/svg"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    version="1.0" width="35pt" height="35pt" viewBox="0 0 128 128"
    xml:space="preserve">
    <g><path d="M59.6 0h8v40h-8V0z" fill="#000"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(30 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(60 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(90 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(120 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#b2b2b2" transform="rotate(150 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#999" transform="rotate(180 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#7f7f7f" transform="rotate(210 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#666" transform="rotate(240 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#4c4c4c" transform="rotate(270 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#333" transform="rotate(300 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#191919" transform="rotate(330 64 64)"/>
    <animateTransform attributeName="transform" type="rotate"
    values="0 64 64;30 64 64;60 64 64;90 64 64;120 64 64;150 64 64;180 64 64;210 64 64;240 64 64;270 64 64;300 64 64;330 64 64"
    calcMode="discrete" dur="1320ms" repeatCount="indefinite">
    </animateTransform></g></svg>"""

def serve_self_refreshing(path : str, title : str, content : str) -> None:
    url = 'http://localhost:' + str(MYXINE_PORT) + '/' + path.strip('/')
    wrapped_content = '<div id="content">' + content + '</div>'
    requests.post(url = url,
                  params = { 'title': title },
                  data = wrapped_content.encode("utf-8"))
