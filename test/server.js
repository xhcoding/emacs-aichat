const http = require("http");

let count = 0;

let server = http.createServer(function(req, res) {
    if (req.url === "/stream") {
        res.writeHead(200, {
            "Content-Type": "text/event-stream",
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "Access-Control-Allow-Origin": '*',
            "Transfer-Encoding": "chunked",
        });
        interval = setInterval(function() {
            res.write("event: test\ndata: this is data\ndata: this is new-line\n\n");
            count++;
            if (count === 3) {
                clearInterval(interval);
                res.socket.end();
                server.close();
            }
        }, 1000);

        req.socket.addListener("close", function() {
            clearInterval(interval);
        }, false);

    }
});

server.listen(12345, "127.0.0.1");
