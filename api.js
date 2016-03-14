
function postnew(body, onSuccess, onError)
{
  $.ajax(
    { url: '/new'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

function postjoin(body, onSuccess, onError)
{
  $.ajax(
    { url: '/join'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

function postdeal(body, onSuccess, onError)
{
  $.ajax(
    { url: '/deal'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

function getdisplay(gameId, onSuccess, onError)
{
  $.ajax(
    { url: '/display/' + encodeURIComponent(gameId) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getplayer(gameId, playerIdx, onSuccess, onError)
{
  $.ajax(
    { url: '/player/' + encodeURIComponent(gameId) + '/' + encodeURIComponent(playerIdx) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getcurrent-bid(gameId, onSuccess, onError)
{
  $.ajax(
    { url: '/current-bid/' + encodeURIComponent(gameId) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function postaction(gameId, body, onSuccess, onError)
{
  $.ajax(
    { url: '/action/' + encodeURIComponent(gameId) + ''
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}
