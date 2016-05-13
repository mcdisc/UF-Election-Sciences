import dropbox
import datetime

dbx = dropbox.Dropbox('kk-_RPmimhUAAAAAAAA0SIHjtNGGPE0TDabT3ETg3JVh-ILOu4SW_953tylAnoQE')

# function to create directory path with year month and day


def create_path():
    date = datetime.date.today()
    # creates path to new directory, I want to have an if statement to see if the directory already exists
    path = '/Election Data Center/Test_Directory/%s%s%s' % (date.year, date.month, date.day)
    return path

# creates the directory using the create_path function


def add_directory(dropbox_object):

    dropbox_object.files_create_folder(create_path())

# creates path to which file will be uploaded


def new_path(file_name):
    # appends create_path to include the name of the file being uploaded
    file_path = create_path() + '/%s' %(file_name)
    return file_path

# uploads file to path created in new_path


def upload_file(dropbox_object, file_from, file_to):
    with open(file_from, 'rb') as f:
        dropbox_object.files_upload(f, file_to)

# creates file name

testfile_name = 'testfile.txt'

# creates directory

add_directory(dbx)


# uploads file


upload_file(dbx, 'testfile.txt', new_path(testfile_name))