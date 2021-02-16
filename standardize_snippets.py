#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os


def get_file_path(path):
    """get all the files' path under the path"""
    file_pathes = []

    lists = os.walk(top=path, topdown=False)
    # print(list(lists))
    for parents, dir_names, file_names in lists:
        for file_name in file_names:
            file_path = os.path.join(parents, file_name)
            file_pathes.append(file_path)


    return file_pathes


def standardize_yasnippet(path):
    """modify .yasnippet file"""
    errorlist = []
    message = ''
    try:
        with open(path, 'r') as f:
            ## add . after key
            lines = f.readlines()
            ## print(lines)
            for line in lines[:]:
                if line[: 6] == '# key:':
                    if line[-2] == '.':
                        pass
                    elif line[-2] == ':':
                        line = line[: -2] + '.\n'
                    else:
                        line = line[: -1] + '.\n'

                message += line

            ## delete last new line
            if '\n' == message[-1]:
                message = message[: -1]

        with open(path, 'w') as f:
            f.write(message)

        ## modify file names
        if '.org' == path[-4:]:
            pass
        elif '.markdown' == path[-9:]:
            pass
        elif '.el' == path[-3:]:
            pass
        elif '.elc' == path[-4:]:
            pass
        elif '.js' == path[-3:]:
            pass
        elif '.gitignore' == path[-10:]:
            pass
        elif '.yas-parents' == path[-12:]:
            pass
        elif '.yas-make-groups' == path[-16:]:
            pass
        elif '.yasnippet' == path[-10:]:
            pass
        elif '.sinppet' == path[-8:]:
            os.rename(path[:-8], path+'.yasnippet')
            path = path[:-8] + '.yasnippet'
        elif '.yasinppet' == path[-10:]:
            os.rename(path[:-10], path+'.yasnippet')
            path = path[:-10] + '.yasnippet'
        elif '.yansnippet' == path[-11:]:
            os.rename(path[:-11], path+'.yasnippet')
            path = path[:-11] + '.yasnippet'
        else:
            os.rename(path, path+'.yasnippet')
            path = path + '.yasnippet'

        num = 0             # 文件路径中点的个数
        for st in path[12:]:
            if st == '.':
                num += 1
        if num > 1:
            name = path[12: -10].replace('.', '_')
            os.rename(path, path[: 12] + name + '.yasnippet')
            path = path[: 12] + name + '.yasnippet'



    except Exception:
        errorlist.append(Exception)

    if errorlist != []:
        print(errorlist)


    return


def main(argv=None):
    if argv is None:
        argv = sys.argv


    path = "./snippets/"
    file_pathes = get_file_path(path)
    # print(file_pathes)

    for file_path in file_pathes:
        standardize_yasnippet(file_path)


    return 0


if __name__ == '__main__':
    sys.exit(main())