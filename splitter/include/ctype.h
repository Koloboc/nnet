#ifndef __CTYPE_H__
#define __CTYPE_H__


#define _C_TYPE_DIGIT						\
	case '0': case '1': case '2': case '3': \
	case '4': case '5': case '6': case '7': \
	case '8': case '9'

static inline bool c_isdigit(int c){
	switch(c){
		_C_TYPE_DIGIT:
			return true;
		default:
			return false;
	}
}

static inline bool c_isspace(int c){
	switch(c){
		case ' ': case '\t': case '\n': case '\v': case '\f': case '\r':
			return true;
		default:
			return false;
	}
}

static inline int is_blank(int c) {
  return (c == ' ' || c == '\t');
}

#endif
