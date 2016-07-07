# Liars Poker Webapp

A multiplayer web application of Liars Poker implementing the [rules](http://www.liars-poker.com/)
from Salomon Brothers 1986. The backend (server) is written in Haskell using Websockets
and the frontend is in elm.

You can try it [here](http://liarspoker.herokuapp.com).

Todo
----------------

- [X] Use wss instead of ws.
- [ ] Use https instead of http.
- [ ] Handle closing of tabs or browser.
- [X] Highlight player with turn.
- [ ] Reveal the total count after a game.
- [ ] Allow old game ids to be reused.
- [ ] Add a sound and/or visual effect after each move.
- [ ] Add Chat feature.
- [ ] Stylize the frontend.
- [ ] Add deployment script.
