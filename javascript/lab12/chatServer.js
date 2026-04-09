const net = require('net');
const eol = require('os').EOL;
const crypto = require('crypto');

const HASH_ALG = 'sha256';
const NAME_LEN = 10;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = calcName(client.remoteAddress, client.remotePort);
  client.write('Welcome, ' + client.name + eol);

  clientList.push(client);

  client.on('data', function(data) {
    const message = data.toString().trim();

    if (!message) {
      return;
    }

    if (message === '\\list') {
      const otherNames = clientList
        .filter(function(otherClient) { return otherClient !== client; })
        .map(function(otherClient) { return otherClient.name; });

      client.write('Users: ' + (otherNames.length ? otherNames.join(', ') : 'none') + eol);
      return;
    }

    if (message.indexOf('\\rename ') === 0) {
      const newName = message.substring('\\rename '.length).trim();

      if (!newName) {
        client.write('Usage: \\rename <newname>' + eol);
        return;
      }

      const oldName = client.name;
      client.name = newName;
      client.write('Renamed to ' + client.name + eol);
      broadcast(client, oldName + ' is now known as ' + client.name);
      return;
    }

    if (message.indexOf('\\private ') === 0) {
      const parts = message.split(' ');

      if (parts.length < 3) {
        client.write('Usage: \\private <name> <msg>' + eol);
        return;
      }

      const targetName = parts[1];
      const privateMessage = message.substring(('\\private ' + targetName + ' ').length);
      const targetClient = clientList.find(function(otherClient) {
        return otherClient.name === targetName;
      });

      if (!targetClient) {
        client.write('No such user: ' + targetName + eol);
        return;
      }

      targetClient.write('[private] ' + client.name + ': ' + privateMessage + eol);
      if (targetClient !== client) {
        client.write('[private] to ' + targetClient.name + ': ' + privateMessage + eol);
      }
      return;
    }

    broadcast(client, client.name + ': ' + message);
  });

  client.on('close', function() {
    clientList = clientList.filter(function(otherClient) {
      return otherClient !== client;
    });
    broadcast(client, client.name + ' left the chat');
  });

  client.on('error', function() {
    client.destroy();
  });

});

function broadcast(sender, message) {
  clientList.forEach(function(client) {
    if (client !== sender) {
      client.write(message + eol);
    }
  });
}

function calcName(remoteAddress, remotePort) {
  let data = remoteAddress + ":" + remotePort;
  let h = crypto.createHash(HASH_ALG).update(data).digest('hex')
  return h.substring(0, NAME_LEN);
}

srvr.listen(9000);