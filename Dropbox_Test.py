import dropbox


class TransferData:
    def __init__(self, access_token):
        self.access_token = access_token

    def upload_file(self, file_from=None, file_to=None):
        """upload a file to Dropbox using API v2
        """
        dbx = dropbox.Dropbox(self.access_token)
        #files_upload(f, path, mode=WriteMode('add', None), autorename=False, client_modified=None, mute=False)

        with open(file_from, 'rb') as f:
            dbx.files_upload(f, file_to)

def main():
    access_token = 'kk-_RPmimhUAAAAAAAA0SIHjtNGGPE0TDabT3ETg3JVh-ILOu4SW_953tylAnoQE'
    transferData = TransferData(access_token)

    file_from = 'testfile.txt'
    file_to = '/Election Data Center/Test_Directory/testfile.txt'  # The full path to upload the file to, including the file name

    # API v2
    transferData.upload_file(file_from=file_from, file_to=file_to)

main()



