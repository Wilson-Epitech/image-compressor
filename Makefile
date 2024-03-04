##
## IMAGE_COMPRESSOR PROJECT, 2024
## image-compressor
## File description:
## Makefile
##

CC = stack build
BINARY_PATH	= $(shell stack path --local-install-root)
OLD_BINARY_NAME = test-exe
NAME = imageCompressor

all:
	$(CC)
	cp $(BINARY_PATH)/bin/$(OLD_BINARY_NAME) ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re