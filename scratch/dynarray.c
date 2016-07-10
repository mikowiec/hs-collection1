
#include <assert.h>
#include <sys/param.h>
#include <stdlib.h>


typedef char int8;

typedef enum ResourceType {
  MT_Normal
} ResourceType;

typedef struct {
} WFX_MemMgr;

void *wfx_nrealloc(WFX_MemMgr *mm, ResourceType rt, int *p, int old_size, int size)
{
  return realloc(p, size);
}

void wfx_free(WFX_MemMgr *mm, ResourceType rt, void *p)
{
  free(p);
}

struct dynarray {
  void *array;
  int capacity;
  int next;
  int8 elem_size;
  ResourceType res_type;
};

int wfx_dynarray_create(WFX_MemMgr* mm, struct dynarray *dynarray, ResourceType rt, int8 elem_size);
int wfx_dynarray_destroy(WFX_MemMgr* mm, struct dynarray *dynarray);
int wfx_dynarray_grow(WFX_MemMgr* mm, struct dynarray *dynarray, void **p);
int wfx_dynarray_shrink(WFX_MemMgr *mm, struct dynarray *dynarray);
int wfx_dynarray_clear(WFX_MemMgr *mm, struct dynarray *dynarray);
int wfx_dynarray_iterate(struct dynarray *dynarray, void **cursor);

int wfx_dynarray_create(WFX_MemMgr* mm, struct dynarray *dynarray, ResourceType rt, int8 elem_size)
{
  dynarray->array = NULL;
  dynarray->capacity = 0;
  dynarray->next = 0;
  dynarray->elem_size = elem_size;
  dynarray->res_type = rt;
  return 0;
}

int wfx_dynarray_destroy(WFX_MemMgr* mm, struct dynarray *dynarray)
{
  wfx_free(mm, dynarray->res_type, dynarray->array);
  dynarray->array = NULL;
  dynarray->capacity = 0;
  dynarray->next = 0;
  dynarray->elem_size = 0;
  dynarray->res_type = 0;
  return 0;
}

int wfx_dynarray_grow(WFX_MemMgr* mm, struct dynarray *dynarray, void **p)
{
  if (dynarray->next >= dynarray->capacity) {
    int new_cap = MAX(dynarray->capacity*2, 32);
    void *new_array = wfx_nrealloc(mm, dynarray->res_type, dynarray->array, dynarray->elem_size*dynarray->capacity, dynarray->elem_size*new_cap);
    if (!new_array)
      return -1;
    dynarray->array = new_array;
    dynarray->capacity = new_cap;
  }
  if (p) {
    *((int8**)p) = ((int8*)dynarray->array) + (dynarray->elem_size*dynarray->next);
  }
  dynarray->next++;
  return 0;
}

int wfx_dynarray_shrink(WFX_MemMgr *mm, struct dynarray *dynarray)
{
  if (dynarray->next == 0)
    return -1;
  dynarray->next--;
  return 0;
}

int wfx_dynarray_iterate(struct dynarray *dynarray, void **cursor)
{
  int8 *p;
  if (!cursor)
    return 1;
  p = *cursor;
  if (!p) {
    p = (int8*)dynarray->array;
  } else {
    p += dynarray->elem_size;
  }
  *cursor = p;
  return ((p - (int8*)dynarray->array) < (dynarray->elem_size*dynarray->next));
}

int wfx_dynarray_clear(WFX_MemMgr *mm, struct dynarray *dynarray)
{
  dynarray->next = 0;
  return 0;
}

typedef struct E {
  int i;
  char c;
} E;

void test0()
{
  WFX_MemMgr mm;
  struct dynarray da;
  wfx_dynarray_create(&mm, &da, MT_Normal, sizeof(E));
  wfx_dynarray_destroy(&mm, &da);
}

void test1()
{
  WFX_MemMgr mm;
  struct dynarray da;
  E *e=0;
  wfx_dynarray_create(&mm, &da, MT_Normal, sizeof(E));
  wfx_dynarray_grow(&mm, &da, (void**)&e);
  assert(e);
  assert(e == da.array);
  assert(da.next == 1);
  assert(da.next <= da.capacity);
  assert(da.elem_size == sizeof(E));
  wfx_dynarray_destroy(&mm, &da);
}

void test2()
{
  WFX_MemMgr mm;
  struct dynarray da;
  wfx_dynarray_create(&mm, &da, MT_Normal, sizeof(E));
  wfx_dynarray_grow(&mm, &da, NULL);
  assert(da.next == 1);
  wfx_dynarray_shrink(&mm, &da);
  assert(da.next == 0);
  wfx_dynarray_destroy(&mm, &da);
}

void test3()
{
  WFX_MemMgr mm;
  struct dynarray da;
  wfx_dynarray_create(&mm, &da, MT_Normal, sizeof(E));
  wfx_dynarray_grow(&mm, &da, NULL);
  wfx_dynarray_clear(&mm, &da);
  assert(da.next == 0);
  wfx_dynarray_destroy(&mm, &da);
}

void test4()
{
  WFX_MemMgr mm;
  struct dynarray da;
  E *e;
  int i;
  wfx_dynarray_create(&mm, &da, MT_Normal, sizeof(E));
  for (i = 0; i < 1000; i++) {
    wfx_dynarray_grow(&mm, &da, (void**)&e);
    e->i = i;
    e->c = '@';
  }
  assert(da.capacity >= 1000);
  i = 0;
  e = 0;
  while (wfx_dynarray_iterate(&da, (void**)&e)) {
    assert(e->i == i);
    assert(e->c == '@');
    i++;
  }
  assert(i == 1000);
  wfx_dynarray_destroy(&mm, &da);
}

int main()
{
  test0();
  test1();
  test2();
  test3();
  test4();

  return 0;
}

