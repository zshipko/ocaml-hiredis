#include "hiredis/hiredis.h"

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <stdio.h>
#include <string.h>

value Some(value x) {
    value dst = caml_alloc(1, 0);
    Store_field(dst, 0, x);
    return dst;
}

#define None Val_int(0)
#define Nil None

value convert_reply(redisReply *reply, int consume){
    value dst = Val_unit;

    if (!reply){
        return Nil;
    }

    if (reply->type == REDIS_REPLY_ERROR){
        dst = caml_alloc(1, 0);
        value s = caml_alloc_string(reply->len);
        memcpy(String_val(s), reply->str, reply->len);
        Store_field(dst, 0, s);
    } else if (reply->type == REDIS_REPLY_STATUS){
        dst = caml_alloc(1, 4);
        value s = caml_alloc_string(reply->len);
        memcpy(String_val(s), reply->str, reply->len);
        Store_field(dst, 0, s);
    } else if (reply->type == REDIS_REPLY_STRING){
        dst = caml_alloc(1, 2);
        value s = caml_alloc_string(reply->len);
        memcpy(String_val(s), reply->str, reply->len);
        Store_field(dst, 0, s);
    } else if (reply->type == REDIS_REPLY_INTEGER){
        dst = caml_alloc(1, 1);
        Store_field(dst, 0, caml_copy_int64(reply->integer));
    } else if (reply->type == REDIS_REPLY_ARRAY){
        dst = caml_alloc(1, 3);
        value s = caml_alloc(reply->elements, 0);
        for(size_t i = 0; i < reply->elements; i++){
            Store_field(s, i, convert_reply(reply->element[i], 0));
        }
        Store_field(dst, 0, s);
    } else {
        dst = Nil;
    }

    if (consume){
        freeReplyObject(reply);
    }

    return dst;
}

redisReply *convert_to_reply(value v){
    redisReply *dst = NULL;
    if (Is_block(v)){
        value val = Field(v, 0);
        switch(Tag_val(v)){
        case 0:
            dst = createReplyObject(REDIS_REPLY_ERROR);
            if (dst){
                dst->len = caml_string_length(val);
                dst->str = strndup(String_val(val), dst->len);
            }
            break;
        case 1:
            dst = createReplyObject(REDIS_REPLY_INTEGER);
            if (dst){
                dst->integer = Int64_val(val);
            }
            break;
        case 2:
            dst = createReplyObject(REDIS_REPLY_STRING);
            if (dst){
                dst->len = caml_string_length(val);
                dst->str = strndup(String_val(val), dst->len);
            }
            break;
        case 3:
            dst = createReplyObject(REDIS_REPLY_ARRAY);
            if (dst){
                dst->elements = Wosize_val(val);
                if (dst->elements > 0){
                    dst->element = calloc(dst->elements, sizeof(redisReply));
                }

                if (dst->element){
                    for(int i = 0; i < dst->elements; i++){
                        dst->element[i] = convert_to_reply(Field(val, i));
                    }
                }
            }
            break;
        case 4:
            dst = createReplyObject(REDIS_REPLY_STATUS);
            if (dst){
                dst->len = caml_string_length(val);
                dst->str = strndup(String_val(val), dst->len);
            }
            break;
        default:
            dst = createReplyObject(REDIS_REPLY_NIL);
        }
    } else {
        dst = createReplyObject(REDIS_REPLY_NIL);
    }

    return dst;
}

value redis_context_of_fd (value _fd){
    int fd = Int_val(_fd);

    if (fd < 0){
        caml_failwith("invalid client");
        return Val_unit;
    }

    redisContext *ctx = redisConnectFd(fd);
    if (!ctx){
        caml_failwith("invalid client");
        return Val_unit;
    }

    return (value)ctx;
}

value redis_context_get_reply(value _reader){
    CAMLparam1(_reader);
    redisReply *reply;
    if (redisGetReply((redisContext*)_reader, (void**)&reply) != REDIS_OK){
        caml_failwith ("invalid reply");
        CAMLreturn(Nil);
    }

    CAMLreturn(convert_reply(reply, 1));
}

value redis_context_connect(value host, value port, value nonblock){
    CAMLparam2(host, port);

    redisContext *context = NULL;
    if (Bool_val(nonblock)){
        context = redisConnectNonBlock(String_val(host), Int_val(port));
    } else {
        context = redisConnect(String_val(host), Int_val(port));
    }

    if (context == NULL){
        caml_failwith ("invalid context");
        CAMLreturn(Val_unit);
    }

    CAMLreturn((value)context);
}

value redis_context_connect_unix(value path, value nonblock){
    CAMLparam1(path);

    redisContext *context = NULL;
    if (Bool_val(nonblock)){
        context = redisConnectUnixNonBlock(String_val(path));
    } else {
        context = redisConnectUnix(String_val(path));
    }

    if (context == NULL){
        caml_failwith ("invalid context");
        CAMLreturn(Val_unit);
    }

    CAMLreturn((value)context);
}

value redis_context_reconnect(value _ctx){
    CAMLparam1(_ctx);
    CAMLreturn(Val_int(redisReconnect((redisContext*)_ctx)));
}

value redis_context_set_timeout (value _ctx, value s, value us){
    CAMLparam1(_ctx);
    struct timeval tv;
    tv.tv_sec = Int_val(s);
    tv.tv_usec = Int_val(us);
    CAMLreturn(Val_int(redisSetTimeout((redisContext*)_ctx, tv)));
}

value redis_context_enable_keepalive(value _ctx){
    CAMLparam1(_ctx);
    CAMLreturn(Val_int(redisEnableKeepAlive((redisContext*)_ctx)));
}

value redis_context_command(value _ctx, value arr){
    CAMLparam2(_ctx, arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    CAMLreturn (convert_reply(redisCommandArgv((redisContext*)_ctx, argc, argv, lens), 1));
}

value redis_context_append_command(value _ctx, value arr){
    CAMLparam2(_ctx, arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    CAMLreturn (Int_val(redisAppendCommandArgv((redisContext*)_ctx, argc, argv, lens)));
}

value redis_context_append_formatted(value _ctx, value s){
    CAMLparam2(_ctx, s);

    CAMLreturn (Int_val(redisAppendFormattedCommand((redisContext*)_ctx, String_val(s), caml_string_length(s))));
}

value redis_format_command(value arr){
    CAMLparam1(arr);

    size_t argc = Wosize_val(arr);
    const char *argv[argc];
    size_t lens[argc];

    for (int i = 0; i < argc; i++){
        argv[i] = String_val(Field(arr, i));
        lens[i] = caml_string_length(Field(arr, i));
    }

    char *dst = NULL;
    int len = redisFormatCommandArgv(&dst, argc, argv, lens);
    if (len < 0){
        redisFreeCommand(dst);
        caml_failwith("invalid command");
        CAMLreturn(Val_unit);
    }

    value s = caml_alloc_string(len);
    memcpy(String_val(s), dst, len);
    redisFreeCommand(dst);

    CAMLreturn(s);
}


value redis_context_free_keep_fd(value _ctx){
    CAMLparam1(_ctx);
    redisFreeKeepFd((redisContext*)_ctx);
    CAMLreturn(Val_unit);
}

value redis_context_free(value _ctx){
    CAMLparam1(_ctx);
    redisFree((redisContext*)_ctx);
    CAMLreturn(Val_unit);
}

value redis_reader_create(value unit){
    redisReader *reader = redisReaderCreate();
    if (!reader){
        caml_failwith("invalid reader");
        return Val_unit;
    }

    return (value)reader;
}

value redis_reader_free(value _reader){
    CAMLparam1(_reader);
    redisReaderFree((redisReader*)_reader);
    CAMLreturn(Val_unit);
}

value redis_reader_feed(value _reader, value s){
    CAMLparam2 (_reader, s);
    CAMLreturn(redisReaderFeed((redisReader*)_reader, String_val(s), caml_string_length(s)));
}

value redis_reader_get_reply(value _reader){
    CAMLparam1(_reader);
    redisReply *reply = NULL;
    if (redisReaderGetReply((redisReader*)_reader, (void**)&reply) != REDIS_OK){
        caml_failwith ("invalid reply");
        CAMLreturn(Nil);
    }

    CAMLreturn(convert_reply(reply, 1));
}

