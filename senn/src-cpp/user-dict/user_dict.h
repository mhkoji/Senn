/*
  TODO:
    sennはuser-dictに依存していないため、
    user-dictはsennが持つよりもkkc-engineが持つほうがよさそう。
 */
#ifndef __SENN_USER_DICT_H__
#define __SENN_USER_DICT_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef void *entry_t;
typedef void *user_dict_t;

user_dict_t user_dict_load(const char *path);

void user_dict_destroy(user_dict_t ud);

int user_dict_count(user_dict_t ud);

entry_t user_dict_entry(user_dict_t ud, int index);

const char *entry_pron(entry_t e);

const char *entry_form(entry_t e);

#ifdef __cplusplus
}
#endif

#endif /* __SENN_USER_DICT_H__ */
