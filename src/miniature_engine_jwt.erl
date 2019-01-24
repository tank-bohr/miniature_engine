-module (miniature_engine_jwt).

-export([
    decode/2
]).

-include_lib("kernel/include/logger.hrl").

decode(Token, Key) ->
    ?LOG_DEBUG("decode(~p, ~p)", [Token, Key]),
    case split_token(Token) of
        SplitToken = [Header, Claims | _] ->
            case decode_jwt(SplitToken) of
                {#{<<"alg">> := Alg} = _Header, ClaimsJSON, Signature} ->
                    case jwt_check_sig(Alg, Header, Claims, Signature, Key) of
                        false -> {error, invalid_signature};
                        true ->
                            case jwt_is_expired(ClaimsJSON) of
                                true  -> {error, expired};
                                false -> {ok, ClaimsJSON}
                            end
                    end;
                invalid ->
                    ?LOG_ERROR("cant be decoded [~p]", [SplitToken]),
                    {error, invalid_token}
            end;
        invalid ->
            ?LOG_ERROR("invalid token: [~p]", [Token]),
            {error, invalid_token}
    end.

jsx_decode_safe(Bin) ->
    try
        jsx:decode(Bin, [return_maps])
    catch _ ->
        invalid
    end.

jwt_is_expired(#{<<"exp">> := Exp} = _ClaimsJSON) ->
    case (Exp - epoch()) of
        DeltaSecs when DeltaSecs > 0 -> false;
        _ -> true
    end;
jwt_is_expired(_) ->
    false.

jwt_check_sig(Alg, Header, Claims, Signature, Key) ->
    jwt_check_sig(algorithm_to_crypto(Alg), <<Header/binary, ".", Claims/binary>>, Signature, Key).

jwt_check_sig({hmac, _} = Alg, Payload, Signature, Key) ->
    ?LOG_DEBUG("jwt_check_sig(~p, ~p, ~p, ~p)", [Alg, Payload, Signature, Key]),
    jwt_sign_with_crypto(Alg, Payload, Key) =:= Signature;
jwt_check_sig({rsa, Crypto}, Payload, Signature, Key) ->
    public_key:verify(Payload, Crypto, base64url:decode(Signature), Key);
jwt_check_sig({ecdsa, Crypto}, Payload, Signature, Key) ->
    ?LOG_DEBUG("jwt_check_sig({ecdsa, ~p}, ~p, ~p, ~p)", [Crypto, Payload, Signature, Key]),
    public_key:verify(Payload, Crypto, base64url:decode(Signature), Key);
jwt_check_sig(_, _, _, _) ->
    false.

split_token(Token) ->
    binary:split(Token, <<".">>, [global]).

decode_jwt([Header, Claims, Signature]) ->
    try
        [HeaderJSON, ClaimsJSON] =
            Decoded = [jsx_decode_safe(base64url:decode(X)) || X <- [Header, Claims]],
        case lists:any(fun(E) -> E =:= invalid end, Decoded) of
            true  -> invalid;
            false -> {HeaderJSON, ClaimsJSON, Signature}
        end
    catch _:_ ->
        invalid
    end;
decode_jwt(_) ->
    invalid.

jwt_sign_with_crypto({hmac,  Crypto}, Payload, Key) ->
    base64url:encode(crypto:hmac(Crypto, Key, Payload));
jwt_sign_with_crypto(_, _Payload, _Key) ->
    undefined.

algorithm_to_crypto(<<"HS256">>) -> {hmac,  sha256};
algorithm_to_crypto(<<"HS384">>) -> {hmac,  sha384};
algorithm_to_crypto(<<"HS512">>) -> {hmac,  sha512};
algorithm_to_crypto(<<"RS256">>) -> {rsa,   sha256};
algorithm_to_crypto(<<"ES256">>) -> {ecdsa, sha256};
algorithm_to_crypto(_)           -> undefined.

epoch() -> erlang:system_time(seconds).