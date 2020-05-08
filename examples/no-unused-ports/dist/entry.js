const app = Elm.Main.init({
    node: document.getElementById('elm')
});
logPorts('ports', app.ports);



function logPorts(log, ports) {
    if (typeof log === 'string') {
        log = document.getElementById(log);
    }
    if (log === null) {
        return;
    }
    if (ports === undefined) {
        createTextElement('p', 'No Ports Available', log);
        return;
    }
    const ul = document.createElement('ul');
    for (port in ports) {
        createTextElement('li', port, ul);
    }
    log.appendChild(ul);
}

function createTextElement(tag, text, parent) {
    const el = document.createElement(tag);
    el.appendChild(document.createTextNode(text));
    if (parent !== undefined) {
        parent.appendChild(el);
    }
    return el;
}