var instance = Elm.embed(Elm.Main, document.getElementById('app'), {
  keyboard: -1
});

window.addEventListener('keydown', function (e) {
  instance.ports.keyboard.send(e.keyCode);
});
