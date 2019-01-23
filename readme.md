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
### Captures:
- «token» to further use it in subscription to web socket

- «response» to start with the given number of posts in table

## WebSocket /posts/subscribe/:token

You access the web socket with the web socket protocol. I. e. `https://` -> `ws://`.

Open a web socket from the given `token` for receiving events about new and deleted posts with `ws://websuck1t.herokuapp.com/posts/subscribe/{token}`

#### Returns error `498` if the supplied token has expired or is invalid.

### Captures:

- *token*: The token, received with the previous request.

### Response:

```javascript
{
    «token»: 1828,
    «response»: {
        «deleted»: [
            1819,
            1835,
            1843
        ],
        «added»: [
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
    «postBody»: «This is the post body»,
    «postAuthor»: 4,
    «postId»: 1
}
```

- Example (`application/json`):

```javascript
{
    «postBody»: «This is another post body»,
    «postAuthor»: 4,
    «postId»: 1
}
```

- Example (`application/json`):

```javascript
{
    «postBody»: «This is the post body»,
    «postAuthor»: 7,
    «postId»: 1
}
```

- Example (`application/json`):

```javascript
{
    «postBody»: «This is another post body»,
    «postAuthor»: 7,
    «postId»: 1
}
```

- Example (`application/json`):

```javascript
{
    «postBody»: «This is the post body»,
    «postAuthor»: 92,
    «postId»: 1
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
