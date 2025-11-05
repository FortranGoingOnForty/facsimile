# Regex Test Patterns

This document lists regex patterns to test with `regex_test_examples.txt`

## Basic Patterns

### Email Addresses
**Pattern:** `[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}`
**Should match:** All valid emails in the Email Addresses section
**Should NOT match:** The invalid email examples

### Phone Numbers (US Format)
**Pattern:** `\([0-9]{3}\) [0-9]{3}-[0-9]{4}`
**Matches:** `(555) 123-4567`

**Pattern:** `\+?[0-9]{1,3}[- ]?[0-9]{3}[- .]?[0-9]{3,4}[- .]?[0-9]{4}`
**Matches:** Most phone number formats

### URLs
**Pattern:** `https?://[a-zA-Z0-9./-]+`
**Matches:** HTTP and HTTPS URLs

**Pattern:** `[a-z]+@[a-z]+\.[a-z]+:[a-z]+/[a-z]+\.git`
**Matches:** Git URLs like `git@github.com:user/repo.git`

### IP Addresses (simple)
**Pattern:** `[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}`
**Matches:** Basic IPv4 addresses

### File Paths
**Pattern:** `/[a-zA-Z0-9/_.-]+`
**Matches:** Unix-style absolute paths

**Pattern:** `\./[a-zA-Z0-9/_.-]+`
**Matches:** Relative paths starting with ./

## Date and Time Patterns

### ISO Date
**Pattern:** `[0-9]{4}-[0-9]{2}-[0-9]{2}`
**Matches:** `2024-01-15`

### ISO DateTime
**Pattern:** `[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z?`
**Matches:** ISO 8601 timestamps

### Time
**Pattern:** `[0-9]{2}:[0-9]{2}(:[0-9]{2})?`
**Matches:** Times like `14:30` or `14:30:00`

## Number Patterns

### Integers
**Pattern:** `[0-9]+`
**Matches:** Any sequence of digits

### Decimals
**Pattern:** `[0-9]+\.[0-9]+`
**Matches:** Numbers with decimal points

### Currency
**Pattern:** `[$€£][0-9]+\.[0-9]{2}`
**Matches:** Currency amounts like `$49.99`

### Hex Colors
**Pattern:** `#[0-9A-Fa-f]{6}`
**Matches:** Hex colors like `#3399CC`

### Scientific Notation
**Pattern:** `[0-9]+\.[0-9]+[eE][+-]?[0-9]+`
**Matches:** Numbers like `1.23e-4`

## Code Patterns

### Function Calls
**Pattern:** `[a-zA-Z_][a-zA-Z0-9_]*\(`
**Matches:** Function names followed by opening parenthesis

### TODO/FIXME Comments
**Pattern:** `(TODO|FIXME|BUG):`
**Matches:** Common code markers

### Constants (SCREAMING_SNAKE_CASE)
**Pattern:** `[A-Z][A-Z0-9_]+`
**Matches:** Constants like `MAX_RETRIES`

### camelCase
**Pattern:** `[a-z]+([A-Z][a-z]+)+`
**Matches:** camelCase identifiers

## Log Patterns

### Log Levels
**Pattern:** `\[(ERROR|WARN|INFO|DEBUG)\]`
**Matches:** Log level tags

### Timestamps in Logs
**Pattern:** `[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}`
**Matches:** Full timestamp in logs

## Version Patterns

### Semantic Version
**Pattern:** `v?[0-9]+\.[0-9]+\.[0-9]+`
**Matches:** Basic semver like `1.2.3` or `v2.0.1`

### With Pre-release
**Pattern:** `[0-9]+\.[0-9]+\.[0-9]+(-[a-z]+)?`
**Matches:** Versions with optional pre-release tag

## HTML/Markdown Patterns

### HTML Tags
**Pattern:** `<[a-z]+[^>]*>`
**Matches:** Opening HTML tags

**Pattern:** `</[a-z]+>`
**Matches:** Closing HTML tags

### Markdown Headers
**Pattern:** `^##? .*$`
**Matches:** Markdown H1 or H2 headers

### Markdown Links
**Pattern:** `\[[^\]]+\]\([^)]+\)`
**Matches:** Markdown link syntax

## Advanced Patterns

### UUID
**Pattern:** `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`
**Matches:** UUIDs

### Git SHA (short)
**Pattern:** `[0-9a-f]{7,40}`
**Matches:** Git commit hashes

### Word Boundaries
**Pattern:** `\bcat\b`
**Matches:** "cat" as whole word only (not in "category")
**Note:** POSIX uses `[[:<:]]cat[[:>:]]` for word boundaries

### Repeated Patterns
**Pattern:** `(ha )+`
**Matches:** One or more "ha " sequences

### Alternation
**Pattern:** `(TODO|FIXME|BUG|HACK)`
**Matches:** Any of the code markers

## Testing Tips

1. **Start simple**: Test basic patterns like `[0-9]+` first
2. **Test escaping**: Patterns with `\.` `\*` `\+` etc.
3. **Test anchors**: Use `^` and `$` for line start/end
4. **Test quantifiers**: `*` (0+), `+` (1+), `?` (0-1), `{n,m}`
5. **Test character classes**: `[abc]`, `[^abc]`, `[a-z]`
6. **Test case sensitivity**: Toggle with Alt-C
7. **Test replacement**: Try replacing matches with regex enabled

## Quick Test Sequence

1. Open file: `./fac scratch_files/regex_test_examples.txt`
2. Press `Ctrl-F` to open search
3. Press `Alt-R` to enable regex mode
4. Try these in order:
   - `[0-9]+` - Find all number sequences
   - `@[a-z]+\.[a-z]+` - Find email domains
   - `\(.*\)` - Find text in parentheses
   - `^##` - Find markdown H2 headers
   - `[A-Z]{3,}` - Find uppercase sequences (3+ letters)
   - `v?[0-9]+\.[0-9]+\.[0-9]+` - Find version numbers
