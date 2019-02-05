# A server example with WebSockets

_url: [`https://websuck1t.herokuapp.com`](https://websuck1t.herokuapp.com)_

# Methods

## GET /posts/all

_start with: [`https://websuck1t.herokuapp.com/posts/all`](https://websuck1t.herokuapp.com/posts/all)_

Use this _url_ to get the token every time when application begins and previous session was closed

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
{
    "token": 1828,
    "response": {
        "deleted": [],
        "added": [
            1844,
            1845,
            1846
        ]
    }
}

```

### Response fields

- "token" to further use it in subscription to web socket
- "response" to start with the given number of posts in table

## socket.io /posts/subscribe/

You access the web socket with the socket io protocol. You will most likely need to install a library.

### Connecting

To start recieving events you send a `"subscribeWithToken"` event with a single `Int` as the messages body. The `Int` should be the last _token_ you recieved from any API.

#### Response

After sending the request, _you_ will receive a `"subscribeWithToken"` event with a single `Bool` in the message of the event. The `Bool` indicates whether you have supplied a valid token. If the response is `true`, you are now subscribed to recieve updates. If thge response was `false`, nothing happens, you will not be sent any updates.

### Receiving updates

Updates will be sent to you on the `"postUpdates"` event. The event message will contain the following JSON Object (same as `/posts/all`):

```javascript
{
    "token": 1828,
    "response": {
        "deleted": [
            1819,
            1835,
            1843
        ],
        "added": [
            1844,
            1845,
            1846
        ]
    }
}
```

## GET /posts/:id

_url: [`https://websuck1t.herokuapp.com/posts/{id}`](https://websuck1t.herokuapp.com/posts/1)_

#### If there is no post with the given `id`, the method returns error `404`.

### Captures:

- *id*: The id of the entity you want to query.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
{
    "postBody": "This is the post body",
    "postAuthor": 4,
    "postId": 1
}
```

- Example (`application/json`):

```javascript
{
    "postBody": "This is another post body",
    "postAuthor": 4,
    "postId": 1
}
```

- Example (`application/json`):

```javascript
{
    "postBody": "This is the post body",
    "postAuthor": 7,
    "postId": 1
}
```

- Example (`application/json`):

```javascript
{
    "postBody": "This is another post body",
    "postAuthor": 7,
    "postId": 1
}
```

- Example (`application/json`):

```javascript
{
    "postBody": "This is the post body",
    "postAuthor": 92,
    "postId": 1
}
```


## GET /users/:id

_url: [`https://websuck1t.herokuapp.com/users/{id}`](https://websuck1t.herokuapp.com/users/1)_

Retrieves a user with the given `id`.

#### If there is no user with the given `id`, the method returns error `404`.

### Captures:

- *id*: The id of the entity you want to query.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
{
    "secondName": "Leonidovich",
    "firstName": "Seva",
    "userId": 1
}
```

- Example (`application/json`):

```javascript
{
    "secondName": "Barovich",
    "firstName": "Foo",
    "userId": 42
}
```

- Example (`application/json`):

```javascript
{
    "secondName": "Fooovich",
    "firstName": "Bar",
    "userId": 69
}
```

- Example (`application/json`):

```javascript
{
    "secondName": "Quixovich",
    "firstName": "Qux",
    "userId": 8
}
```
