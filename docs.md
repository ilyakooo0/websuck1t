## GET /posts/:id

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
    "reponse": [
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 194
        }
    ]
}
```

- Example (`application/json`):

```javascript
{
    "token": 46,
    "reponse": [
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 194
        }
    ]
}
```

- Example (`application/json`):

```javascript
{
    "token": 193,
    "reponse": [
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 194
        }
    ]
}
```

- Example (`application/json`):

```javascript
{
    "token": 7,
    "reponse": [
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 1
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 4
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 7
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 4,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 7,
            "postId": 194
        },
        {
            "postBody": "This is the post body",
            "postAuthor": 92,
            "postId": 194
        },
        {
            "postBody": "This is another post body",
            "postAuthor": 92,
            "postId": 194
        }
    ]
}
```

## GET /posts/subscribe/:token

### title

not title

### Captures:

- *token*: The token, recieved wit the previuos request

### Response:

- Status code 200
- Headers: []

- No response body

## GET /users/:id

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

## GET /users/add/:first/:second

### Captures:

- *first*: The first name of the user.
- *second*: The second name of the user.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
[]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Barovich",
            "firstName": "Foo"
        },
        "userId": 42
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

## GET /users/all

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Example (`application/json`):

```javascript
[]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Barovich",
            "firstName": "Foo"
        },
        "userId": 42
    }
]
```

- Example (`application/json`):

```javascript
[
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    },
    {
        "userName": {
            "secondName": "Leonidovich",
            "firstName": "Seva"
        },
        "userId": 1
    }
]
```

