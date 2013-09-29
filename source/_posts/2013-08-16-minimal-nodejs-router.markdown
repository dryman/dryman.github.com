---
layout: post
title: "Minimal NodeJS router"
date: 2013-08-16 15:41
comments: true
categories: NodeJS
---

Here comes the problem, you're prototyping a website that has powerful front-end like [EmberJS][] or [AngularJS][], and sync JSON data with your NodeJS back-end, but you want your nodejs code to be lite and clean.

You can use some nodeJS framework like restify, expressJS, director or whatever, but is there a way to write a minimal router using regex and switch statements? Yes.

[EmberJS]: http://emberjs.com

[AngularJS]: http://angularjs.org


<!--more-->

The solution is pretty simple. JS switch statement can accept expressions as comparison keys, so simply do this:

```js
var rest = require('./rest_route.js');
Http.createServer(function(request,response){
  var url = request.url;
  switch(true){
    case RegExp('/api/1/servers$').test(url):
      rest.servers(request,response);
      break;
    case RegExp('/api/1/servers/\\d').test(url):
      rest.server(request,response); 
      break;
    case RegExp('/api/1/nodes').test(url):
      rest.nodes(request,response); 
      break;
    case RegExp('/api/1/url/').test(url):
      rest.urlapi(request,response); 
      break;
    default:
      response.end();
  }
}).listen(3000);
```

This may not as consistent as other heavy routers, but it's really suitable for prototyping a new web app. Have fun and use it to hack your new weekend project!
