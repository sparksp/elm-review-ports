# Unused Ports

This example was created to explore the dead code elimination of ports in Elm - it does not demonstrate the rule.  Update [`src/Main.elm`](./src/Main.elm) and then use `yarn run build` to refresh `dist/main.js` - when you view [`dist/index.html`](./dist/index.html) you should see what ports are available to JavaScript.


## Findings

  - It does not matter whether the port is returned directly or via other function calls.
  - Being exposed from a module has no effect on its own.
  - A port is only made available when:
      - An outgoing port is returned from an application's `update`.
      - An incoming port is returned from an application's `subscriptions`.
  - When there are no available ports, `app.ports` is undefined.
  - There is no difference with or without `--optimize`.
