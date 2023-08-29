import qrcode
from PIL import Image, ImageDraw

# Define the data you want to encode
data = "https://github.com/C-Monaghan/Procrastination_SEM"

# Generate the QR code
qr = qrcode.QRCode(
    version=1,
    error_correction=qrcode.constants.ERROR_CORRECT_H,
    box_size=10,
    border=4,
)
qr.add_data(data)
qr.make(fit=True)
qr_img = qr.make_image(fill_color="black", back_color="white")

# Create a new image with a blank space in the center
image_width, image_height = qr_img.size
blank_space_size = 150  # Adjust this to control the size of the blank space

blank_img = Image.new("RGB", qr_img.size, "white")
blank_img.paste(qr_img, (0, 0))

# Define the coordinates for the blank space
center_x, center_y = image_width // 2, image_height // 2
blank_left = center_x - blank_space_size // 2
blank_upper = center_y - blank_space_size // 2
blank_right = center_x + blank_space_size // 2
blank_lower = center_y + blank_space_size // 2

# Draw a white rectangle in the center to create the blank space
draw = ImageDraw.Draw(blank_img)
draw.rectangle([(blank_left, blank_upper), (blank_right, blank_lower)], fill = "white")
 
# Save or display the final image
blank_img.save("03__QR/images/QR_code.png")
blank_img.show()
