**Prompt:**
You are to write a commit message based on the provided file list and diffs.
Wait until I say **END OF INPUT** before responding.

**Format:**

```
<type>: <subject>

<body>
```

**Rules:**

* `type`: one of `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`
* Subject:
  * 50–70 characters
  * Imperative mood
  * No ending period
  * No “and”

* Body:
  * Bullet points
  * Past tense
  * State **what** and **why** only (no “how”)
  * Skip obvious reasons
  * Skip really minor changes that is not important to mentions.
  * no need to use things like "for better visual clarity" for showing something.
    For example, if you wanted to say "* Updated header to use emojis for better visual clarity." skip the "for better visual clarity" part.

* Small changes → use `fix`

**Output:** Commit message only — no extra text.

**Input:**

```
__INPUT__
```