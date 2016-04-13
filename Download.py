from ftplib import FTP
import zipfile

ftp = FTP('sosftp.sos.state.oh.us')

ftp.login()

ftp.cwd('free')

ftp.cwd('Voter')

ftp.retrlines('LIST')

ftp.retrbinary('RETR VINTON.zip', open('VINTON.zip', 'wb').write)

ftp.quit()

with zipfile.ZipFile('VINTON.zip') as txt:
    txt.extractall()

