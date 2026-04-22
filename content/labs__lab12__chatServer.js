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

  //
  // **YOUR CODE HERE**
  //
  // First, add the client to clientList.
  //
  // Next, add a listener to the client for a 'data' event.
  // This event means that the client typed something in.
  // Broadcast that message to all **other** clients in clientList.
  //
  // For extra challenges, try adding the following functionality:
  // 1) Typing '\list' will list the names of all other users.
  // 2) Typing '\rename <newname>' will let the user specify a new name for himself/herself.
  // 3) Typing '\private <name> <msg>' will send a message only to the specified user.

});

function calcName(remoteAddress, remotePort) {
  let data = remoteAddress + ":" + remotePort;
  let h = crypto.createHash(HASH_ALG).update(data).digest('hex')
  return h.substring(0, NAME_LEN);
}

srvr.listen(9000);


