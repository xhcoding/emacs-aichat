const http = require("http");

let server = http.createServer(function(req, res) {
    if (req.url === "/stream") {
        let count = 0;
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

    } else {
        let params = {};
        for (const [key, value] of new URLSearchParams(new URL(req.url, `http://${req.headers.host}`).search)) {
            params[key] = value;
        }

        let headers = req.headers;
        let body = [];

        req.on("data", (chunk) => {
            body.push(chunk);
        }).on("end", () => {
            const response = {
                params,
                headers,
                body: body.toString(),
            };
            res.setHeader('Content-Type', 'application/json');
            res.end(JSON.stringify(response));
        });


    } 
    
});

server.listen(12345, "127.0.0.1");
