# Introduction

Plug is a plugin manager to download, update, and load systems into your running application.

# Usage

Set the plugin directory:

```lisp
(setf plug:*plugin-directory* #p"/tmp/plug/")
(initialize-plug)
```

Clone a plugin:

```lisp
(plug:clone-plugin "https://github.com/user/plugin")
```

Load into application:

```lisp
(plug:load-plugin-system :system-name)
```

Update plugins:

```lisp
(plug:update-all-plugins)
```
