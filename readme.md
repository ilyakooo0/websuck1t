# A server example with WebSockets

_url: [`https://websuck1t.herokuapp.com`](https://websuck1t.herokuapp.com)_

# Methods

## GET /posts/all

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
{
    "token": 1,
    "response": [
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 2
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 3
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 5
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 6
        }
    ]
}
```

## GET /users/:id

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
    "userName": {
        "secondName": "Leonidovich",
        "firstName": "Seva"
    },
    "userId": 1
}
```

- Example (`application/json`):

```javascript
{
    "userName": {
        "secondName": "Barovich",
        "firstName": "Foo"
    },
    "userId": 42
}
```

- Example (`application/json`):

```javascript
{
    "userName": {
        "secondName": "Fooovich",
        "firstName": "Bar"
    },
    "userId": 69
}
```

- Example (`application/json`):

```javascript
{
    "userName": {
        "secondName": "Quixovich",
        "firstName": "Qux"
    },
    "userId": 8
}
```

## WebSocket /posts/subscribe/:token

You access the web socket with the web socket protocol. I. e. `https://` -> `ws://`.

Open a web socket from the given `token` for receiving events about new and deleted posts.

#### Returns error `498` if the supplied token has expired or is invalid.


### Captures:

- *token*: The token, received with the previous request.

### Response:

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
