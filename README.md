# EmojiAPI

## Terminology

**Emoji** (aka emoticons) are ideograms and smileys used in electronic messages and web pages.

- An **emoji shortcode** is an ASCII string starting and ending with `:` (colon) that denotes an emoji. 
  - For instance, `:smiley:` denotes 😃, and `:facepalm:` denotes 🤦.

- We represent emoji as binaries with the UTF-8 encoding of the emoji. 
  - For instance, the binary `<<240,159,152,131>>` represents 😃 and `<<240,159,164,166>>` represents 🤦.

- An **analytics function** is a function that takes a shortcode and some state and returns an updated state. 

### Types:

``
-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State). ``

## The Emoji Module

Implement an Erlang module `emoji` with the following API, where `E` always stands for an Emoji server:

### `start(Initial) 😄`
- **Purpose:** For starting an Emoji server, with the initial shortcodes definition given by `Initial`.
- **Returns:** `{ok, E}` on success, or `{error, Reason}` if some error occurred.

### `new_shortcode(E, Short, Emo) 😃`
- **Purpose:** For registering that the shortcode `Short` denotes the emoji `Emo`.
- **Returns:** `ok` on success or `{error, Reason}` if some error occurred.

### `alias(E, Short1, Short2) 🙌`
- **Purpose:** For registering `Short2` as an alias for `Short1`.
- **Returns:** `ok` on success or `{error, Reason}` if some error occurred.

### `delete(E, Short) 💥`
- **Purpose:** Removes any denotations for `Short`.

### `lookup(E, Short) 👀`
- **Purpose:** For looking up the emoji for `Short`.
- **Returns:** `{ok, Emo}` on success, or `no_emoji` if no shortcode (including aliases) are found.

### `analytics(E, Short, Fun, Label, Init) 📈`
- **Purpose:** For registering an analytics function for `Short` (and aliases).
- **Returns:** `ok` on success or `{error, Reason}` if some error occurred.

### `get_analytics(E, Short) 📉`
- **Purpose:** For getting the result of all analytics functions associated with a specific shortcode (including aliases).
- **Returns:** `{ok, Stat}` on success, where `Stat` is a list `[{string, any()}]` with the label and the (updated) state for each analytics function registered for `Short`; otherwise returns `{error, Reason}` if some error occurred.

### `remove_analytics(E, Short, Label) 🔪`
- **Purpose:** For removing the analytics function registered under `Label` for `Short` (including aliases).

### `stop(E) :godmode:`
- **Purpose:** For stopping an emoji server, and all associated processes (if relevant).
- **Returns:** `ok` once all processes associated with `E` has terminated; or `{error, Reason}` if some error occurred.
