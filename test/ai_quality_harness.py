#!/usr/bin/env python3
"""
Reported, non-gating quality harness for completion prompts.

Models vary run to run, so this never fails the build. Its job is to make a
prompt change *comparable*: run it before and after and see whether the
completions got better or worse on the same fixtures.

Two things are measured per fixture:
  hit      - the completion contains the expected token(s)
  overrun  - the completion ran past the end of the construct, which is the
             dominant failure of a base FIM model asked for many tokens and
             the thing stop sequences exist to fix

Usage:
  python3 test/ai_quality_harness.py            # with context (default)
  python3 test/ai_quality_harness.py --bare     # window only, no header/digest/stops
"""
import json
import sys
import urllib.request

HOST = "http://127.0.0.1:11434"
MODEL = "qwen2.5-coder:1.5b-base"

# (name, prefix, suffix, filename, expected substrings, overrun markers)
CASES = [
    ("comment-driven max",
     "/* Return the larger of a and b. */\nint max_of(int a, int b) {\n    ",
     "}\n", "m.c", ["a", "b"], ["int ", "void "]),

    ("loop body",
     "/* Sum every element. */\nint total(const int *a, int n) {\n"
     "    int sum = 0;\n    ",
     "    return sum;\n}\n", "t.c", ["for", "sum"], ["int other", "\n}\n\n"]),

    ("python accumulate",
     "def total(values):\n    s = 0\n    for v in values:\n        ",
     "    return s\n", "t.py", ["s"], ["def ", "class "]),

    # The filler here deliberately exceeds the 6000-byte prefix window, so
    # the definition really is out of sight and only the symbol digest can
    # carry it. This is the case the digest exists for.
    ("function beyond the prefix window",
     "int helper_far_away(int x) { return x * 2; }\n"
     + "\n".join("static int filler_%03d(void) { return %d; }" % (i, i)
                  for i in range(180))
     # The comment must NOT name the function, or it leaks the answer into
     # the window and the digest is not what is being measured.
     + "\n/* Double n using the helper defined at the top of this file. */\n"
       "int use(int n) {\n    return ",
     ";\n}\n", "u.c", ["helper_far_away"], ["int ", "void "]),

    ("fortran subroutine",
     "! Set every element of a to zero.\nsubroutine zero(a, n)\n"
     "    real, intent(inout) :: a(:)\n    integer, intent(in) :: n\n    integer :: i\n    ",
     "end subroutine zero\n", "z.f90", ["do", "a("], ["subroutine ", "function "]),
]

STOPS = {
    ".c": ["\n}", "\n\n\n"],
    ".py": ["\ndef ", "\nclass ", "\n\n\n"],
    ".f90": ["\nend subroutine", "\nend function", "\n\n\n"],
}


def ext_of(name):
    return name[name.rfind("."):]


PREFIX_WINDOW = 6000


def window(prefix):
    """Cut to the prefix budget on a line boundary, as the real module does."""
    if len(prefix) <= PREFIX_WINDOW:
        return prefix
    cut = prefix[-PREFIX_WINDOW:]
    nl = cut.find("\n")
    return cut[nl + 1:] if nl >= 0 else cut


def digest_and_header(prefix, filename, bare):
    """Mirror what completion_prompt_module prepends, including the window
    cut -- without it the comparison is not fair, because the bare run would
    see context the real bare path never gets."""
    if bare:
        return window(prefix)
    tok = "#" if filename.endswith(".py") else ("!" if filename.endswith(".f90") else "//")
    head = "%s %s\n" % (tok, filename)
    syms = []
    for ln in prefix.split("\n"):          # digest scans the WHOLE file
        if not ln or ln[0] in " \t":
            continue
        t = ln.rstrip()
        looks_def = (t.endswith("{") or t.endswith(")")
                     or ("(" in t and (t.endswith("}") or t.endswith(";"))))
        if looks_def or ln.startswith("def ") or ln.startswith("subroutine "):
            syms.append("%s %s\n" % (tok, " ".join(ln.split())))
    return head + "".join(syms[:20]) + window(prefix)


def run(prefix, suffix, filename, bare):
    opts = {"num_predict": 96, "temperature": 0.1, "repeat_penalty": 1.05}
    if not bare:
        opts["stop"] = STOPS.get(ext_of(filename), ["\n\n\n"])
    body = json.dumps({
        "model": MODEL,
        "prompt": digest_and_header(prefix, filename, bare),
        "suffix": suffix,
        "stream": False,
        "keep_alive": "10m",
        "options": opts,
    }).encode()
    req = urllib.request.Request(HOST + "/api/generate", data=body,
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r).get("response", "")


def main():
    bare = "--bare" in sys.argv
    try:
        urllib.request.urlopen(HOST + "/api/tags", timeout=3)
    except Exception:
        print("SKIP: no ollama on 127.0.0.1:11434")
        return 0

    mode = "BARE (window only)" if bare else "WITH CONTEXT (header + digest + stops)"
    print("=== %s | %s ===" % (mode, MODEL))
    hits = overruns = 0
    for name, prefix, suffix, fname, expect, bad in CASES:
        try:
            out = run(prefix, suffix, fname, bare)
        except Exception as e:
            print("  %-34s ERROR %s" % (name, e))
            continue
        hit = all(e in out for e in expect)
        over = any(b in out for b in bad)
        hits += hit
        overruns += over
        flag = ("hit " if hit else "MISS") + ("/OVERRUN" if over else "")
        first = out.strip().split("\n")[0][:52]
        print("  %-34s %-12s %r" % (name, flag, first))

    print("\n  hits %d/%d   overruns %d/%d" % (hits, len(CASES), overruns, len(CASES)))
    print("  (reported only -- models vary, this never fails the build)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
