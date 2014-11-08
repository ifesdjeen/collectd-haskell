#ifndef CUSTOM_H
#define CUSTOM_H

#define DATA_MAX_NAME_LEN 64

struct custom_s
{
	char           name[DATA_MAX_NAME_LEN];
	int            i;
};

typedef struct custom_s custom_t;

#endif /* CUSTOM_H */
