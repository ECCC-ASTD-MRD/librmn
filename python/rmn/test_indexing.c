#include "indexing.h"
#include <dirent.h>
#include <regex.h>
const int MAX_FILES = 2000;

int findFilesMatchingRegex(const char *dirPath, const char *pattern, char **paths, int *nbPaths);
int main(int argc, char **argv){

    if(argc != 3){
        fprintf(stderr, "%s: ERROR: Incorrect number of arguments\n\n\t%s DIR REGEX\n\n", argv[0], argv[0]);
        return 1;
    }
    const char *dirPath = argv[1];
    const char *pattern = argv[2];

    char *filenames[MAX_FILES];
    int nb_files;
    int err = findFilesMatchingRegex(dirPath, pattern, filenames, &nb_files);
    if(err){
        return 1;
    }
    fprintf(stderr, "Found %d files matching the regex '%s' in directory '%s'\n", nb_files, pattern, dirPath);

    RecordData *raw_columns = rmn_get_index_columns_raw((const char **)filenames, nb_files);
    if(raw_columns == NULL){
        return 1;
    }

    for(int i = 0; i < nb_files; i++){
        free(filenames[i]);
    }
    free_record_data(raw_columns);
}

int findFilesMatchingRegex(const char *dirPath, const char *pattern, char **paths, int *nb_paths)
{
    DIR *dir = opendir(dirPath);
    if (!dir) {
        fprintf(stderr, "opendir failed %s\n", dirPath);
        return 1;
    }

    regex_t regex;
    regmatch_t rm[8];
    int err = regcomp(&regex, pattern, REG_EXTENDED);
    if (err) {
        fprintf(stderr, "Could not compile regex\n");
        return 1;
    }

    struct dirent *entry;
    int i = 0;
    while(((entry = readdir(dir)) != NULL) && (i < MAX_FILES) ){

        char file_path[PATH_MAX];
        snprintf(file_path, PATH_MAX, "%s/%s", dirPath, entry->d_name);

        int err = regexec(&regex, entry->d_name, 0, NULL, 0);
        if(err == REG_NOMATCH){
            continue;
        }

        paths[i] = malloc(PATH_MAX * sizeof(*paths[i]));
        strncpy(paths[i], file_path, PATH_MAX - 1); // Copy the path
        i++;
    }
    *nb_paths = i;

    regfree(&regex);
    closedir(dir);
    return 0;
}
