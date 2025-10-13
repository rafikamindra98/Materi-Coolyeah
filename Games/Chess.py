import pygame
import sys
import random
import json
import math
import time
from enum import Enum
from copy import deepcopy

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 1200
WINDOW_HEIGHT = 800
BOARD_SIZE = 640
CELL_SIZE = BOARD_SIZE // 8
BOARD_OFFSET_X = 50
BOARD_OFFSET_Y = 80

# Enhanced Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
CREAM = (240, 217, 181)
BROWN = (181, 136, 99)
DARK_BROWN = (139, 69, 19)
LIGHT_BROWN = (222, 184, 135)
LIGHT_GREEN = (144, 238, 144)
DARK_GREEN = (0, 100, 0)
GOLD = (255, 215, 0)
SILVER = (192, 192, 192)
RED = (220, 20, 60)
BLUE = (70, 130, 180)
PURPLE = (147, 112, 219)
ORANGE = (255, 140, 0)
GRAY = (128, 128, 128)
LIGHT_GRAY = (211, 211, 211)
DARK_GRAY = (64, 64, 64)

# Piece colors
PIECE_WHITE = (240, 240, 240)
PIECE_BLACK = (40, 40, 40)
PIECE_WHITE_SHADOW = (180, 180, 180)
PIECE_BLACK_SHADOW = (20, 20, 20)

class PieceType(Enum):
    KING = "king"
    QUEEN = "queen"
    ROOK = "rook"
    BISHOP = "bishop"
    KNIGHT = "knight"
    PAWN = "pawn"

class PieceColor(Enum):
    WHITE = "white"
    BLACK = "black"

class GameMode(Enum):
    MENU = "menu"
    DIFFICULTY_SELECT = "difficulty_select"
    COLOR_SELECT = "color_select"
    HUMAN_VS_HUMAN = "human_vs_human"
    HUMAN_VS_AI = "human_vs_ai"
    GAME_OVER = "game_over"

class Difficulty(Enum):
    EASY = ("Easy", 1, "Random moves with basic evaluation")
    MEDIUM = ("Medium", 3, "Strategic play with 3-move lookahead")
    HARD = ("Hard", 5, "Advanced AI with 5-move lookahead")

class AnimatedPiece:
    def __init__(self, piece, start_pos, end_pos):
        self.piece = piece
        self.start_x = start_pos[0]
        self.start_y = start_pos[1]
        self.end_x = end_pos[0]
        self.end_y = end_pos[1]
        self.current_x = self.start_x
        self.current_y = self.start_y
        self.progress = 0.0
        self.duration = 0.3
        self.start_time = time.time()
        self.rotation = 0.0
        
    def update(self):
        elapsed = time.time() - self.start_time
        self.progress = min(1.0, elapsed / self.duration)
        
        # Smooth easing function
        t = self.progress
        eased_progress = 1 - (1 - t) ** 3
        
        # Update position
        self.current_x = self.start_x + (self.end_x - self.start_x) * eased_progress
        self.current_y = self.start_y + (self.end_y - self.start_y) * eased_progress
        
        # Add subtle rotation during movement
        self.rotation = math.sin(self.progress * math.pi) * 10
        
        return self.progress >= 1.0

class Piece:
    def __init__(self, piece_type, color, row, col):
        self.type = piece_type
        self.color = color
        self.row = row
        self.col = col
        self.has_moved = False
        self.value = self.get_piece_value()
        
    def get_piece_value(self):
        values = {
            PieceType.PAWN: 100,
            PieceType.KNIGHT: 300,
            PieceType.BISHOP: 320,
            PieceType.ROOK: 500,
            PieceType.QUEEN: 900,
            PieceType.KING: 10000
        }
        return values.get(self.type, 0)

class Particle:
    def __init__(self, x, y, color, velocity=(0, 0), life=60):
        self.x = x
        self.y = y
        self.color = color
        self.velocity = velocity
        self.life = life
        self.max_life = life
        self.size = random.randint(3, 8)
        self.spin = random.uniform(0, 2 * math.pi)
        self.spin_speed = random.uniform(-0.3, 0.3)
        
    def update(self):
        self.x += self.velocity[0]
        self.y += self.velocity[1]
        self.velocity = (self.velocity[0] * 0.95, self.velocity[1] + 0.15)
        self.life -= 1
        self.spin += self.spin_speed
        
    def draw(self, screen):
        if self.life > 0:
            alpha = int(255 * (self.life / self.max_life))
            size = max(1, int(self.size * (self.life / self.max_life)))
            
            points = []
            for i in range(6):
                angle = self.spin + i * math.pi / 3
                px = self.x + size * math.cos(angle)
                py = self.y + size * math.sin(angle)
                points.append((px, py))
            
            if len(points) >= 3:
                surface = pygame.Surface((size * 4, size * 4), pygame.SRCALPHA)
                color_with_alpha = (*self.color, alpha)
                adjusted_points = [(p[0] - self.x + size * 2, p[1] - self.y + size * 2) for p in points]
                pygame.draw.polygon(surface, color_with_alpha, adjusted_points)
                screen.blit(surface, (self.x - size * 2, self.y - size * 2))

class PieceRenderer:
    """Class to render beautiful chess pieces"""
    
    @staticmethod
    def draw_piece_shadow(screen, x, y, size, offset=3):
        """Draw shadow for piece"""
        shadow_surface = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow_surface, (0, 0, 0, 50), 
                           (0, size + offset, size * 2, size // 2))
        screen.blit(shadow_surface, (x - size, y - size))
    
    @staticmethod
    def draw_pawn(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed pawn"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base
        base_rect = pygame.Rect(x - size//3, y + size//2, size//1.5, size//4)
        pygame.draw.ellipse(screen, shadow_color, 
                           (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.ellipse(screen, piece_color, base_rect)
        
        # Body
        body_rect = pygame.Rect(x - size//4, y - size//4, size//2, size//1.2)
        pygame.draw.ellipse(screen, shadow_color, 
                           (body_rect.x + 2, body_rect.y + 2, body_rect.width, body_rect.height))
        pygame.draw.ellipse(screen, piece_color, body_rect)
        
        # Head
        head_radius = size // 5
        pygame.draw.circle(screen, shadow_color, (x + 2, y - size//3 + 2), head_radius)
        pygame.draw.circle(screen, piece_color, (x, y - size//3), head_radius)
        
        # Highlight
        pygame.draw.circle(screen, WHITE if color == PieceColor.WHITE else GRAY, 
                          (x - 3, y - size//3 - 3), head_radius // 3)
    
    @staticmethod
    def draw_rook(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed rook (castle)"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base
        base_rect = pygame.Rect(x - size//2.5, y + size//3, size//1.25, size//3)
        pygame.draw.rect(screen, shadow_color, 
                        (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.rect(screen, piece_color, base_rect)
        
        # Main tower
        tower_rect = pygame.Rect(x - size//3, y - size//2, size//1.5, size)
        pygame.draw.rect(screen, shadow_color,
                        (tower_rect.x + 2, tower_rect.y + 2, tower_rect.width, tower_rect.height))
        pygame.draw.rect(screen, piece_color, tower_rect)
        
        # Castle battlements
        battlement_width = size // 6
        for i in range(3):
            batt_x = x - size//3 + i * battlement_width + i * 2
            batt_rect = pygame.Rect(batt_x, y - size//2, battlement_width, size//4)
            pygame.draw.rect(screen, piece_color, batt_rect)
            # Highlight
            pygame.draw.line(screen, WHITE if color == PieceColor.WHITE else GRAY,
                           (batt_x, y - size//2), (batt_x, y - size//4), 2)
    
    @staticmethod
    def draw_knight(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed knight (horse)"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base
        base_rect = pygame.Rect(x - size//2.5, y + size//3, size//1.25, size//4)
        pygame.draw.ellipse(screen, shadow_color,
                           (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.ellipse(screen, piece_color, base_rect)
        
        # Horse head profile
        head_points = [
            (x - size//4, y + size//4),      # neck base
            (x - size//6, y - size//3),      # back of head
            (x + size//6, y - size//2),      # top of head
            (x + size//3, y - size//4),      # nose
            (x + size//4, y),                # mouth
            (x, y + size//6),                # chin
            (x - size//5, y + size//4)       # neck front
        ]
        
        # Draw shadow
        shadow_points = [(p[0] + 2, p[1] + 2) for p in head_points]
        pygame.draw.polygon(screen, shadow_color, shadow_points)
        pygame.draw.polygon(screen, piece_color, head_points)
        
        # Horse ear
        ear_points = [
            (x - size//8, y - size//3),
            (x, y - size//2.5),
            (x + size//12, y - size//4)
        ]
        pygame.draw.polygon(screen, piece_color, ear_points)
        
        # Eye
        pygame.draw.circle(screen, BLACK, (x + size//8, y - size//6), 2)
        
        # Mane details
        for i in range(3):
            mane_y = y - size//4 + i * size//8
            pygame.draw.line(screen, shadow_color,
                           (x - size//6, mane_y), (x - size//10, mane_y - size//10), 2)
    
    @staticmethod
    def draw_bishop(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed bishop"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base
        base_rect = pygame.Rect(x - size//2.5, y + size//3, size//1.25, size//4)
        pygame.draw.ellipse(screen, shadow_color,
                           (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.ellipse(screen, piece_color, base_rect)
        
        # Main body (mitre shape)
        body_points = [
            (x, y + size//3),                # bottom
            (x - size//4, y),                # left middle
            (x - size//6, y - size//3),      # left top
            (x, y - size//2),                # peak
            (x + size//6, y - size//3),      # right top
            (x + size//4, y),                # right middle
        ]
        
        # Draw shadow
        shadow_points = [(p[0] + 2, p[1] + 2) for p in body_points]
        pygame.draw.polygon(screen, shadow_color, shadow_points)
        pygame.draw.polygon(screen, piece_color, body_points)
        
        # Bishop's slit
        slit_rect = pygame.Rect(x - 1, y - size//3, 2, size//4)
        pygame.draw.rect(screen, shadow_color, slit_rect)
        
        # Cross on top
        cross_size = size // 8
        pygame.draw.line(screen, shadow_color,
                        (x - cross_size, y - size//2), (x + cross_size, y - size//2), 3)
        pygame.draw.line(screen, shadow_color,
                        (x, y - size//2 - cross_size//2), (x, y - size//2 + cross_size//2), 3)
        
        # Highlight
        pygame.draw.line(screen, WHITE if color == PieceColor.WHITE else GRAY,
                        (x - size//8, y - size//6), (x - size//8, y + size//8), 2)
    
    @staticmethod
    def draw_queen(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed queen"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base
        base_rect = pygame.Rect(x - size//2, y + size//3, size, size//3)
        pygame.draw.ellipse(screen, shadow_color,
                           (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.ellipse(screen, piece_color, base_rect)
        
        # Main body
        body_points = [
            (x, y + size//3),                # bottom center
            (x - size//3, y + size//6),      # bottom left
            (x - size//4, y - size//6),      # middle left
            (x, y - size//4),                # top center
            (x + size//4, y - size//6),      # middle right
            (x + size//3, y + size//6),      # bottom right
        ]
        
        # Draw shadow
        shadow_points = [(p[0] + 2, p[1] + 2) for p in body_points]
        pygame.draw.polygon(screen, shadow_color, shadow_points)
        pygame.draw.polygon(screen, piece_color, body_points)
        
        # Crown spikes
        crown_points = []
        num_spikes = 5
        for i in range(num_spikes):
            angle = (i - 2) * 0.5  # Spread spikes
            spike_height = size//3 + (size//6 if i % 2 == 0 else 0)  # Alternating heights
            spike_x = x + math.sin(angle) * size//4
            spike_y = y - size//4 - spike_height
            crown_points.append((spike_x, spike_y))
        
        # Connect crown spikes
        for i, point in enumerate(crown_points):
            base_x = x + math.sin((i - 2) * 0.5) * size//4
            base_y = y - size//4
            pygame.draw.line(screen, piece_color, (base_x, base_y), point, 3)
            # Crown jewels
            pygame.draw.circle(screen, GOLD if color == PieceColor.WHITE else SILVER, 
                             (int(point[0]), int(point[1])), 2)
        
        # Central jewel
        pygame.draw.circle(screen, RED, (x, y - size//8), 3)
        pygame.draw.circle(screen, WHITE, (x - 1, y - size//8 - 1), 1)
    
    @staticmethod
    def draw_king(screen, x, y, color, size=30, rotation=0):
        """Draw a detailed king"""
        piece_color = PIECE_WHITE if color == PieceColor.WHITE else PIECE_BLACK
        shadow_color = PIECE_WHITE_SHADOW if color == PieceColor.WHITE else PIECE_BLACK_SHADOW
        
        # Shadow
        PieceRenderer.draw_piece_shadow(screen, x, y, size)
        
        # Base (larger than other pieces)
        base_rect = pygame.Rect(x - size//2, y + size//3, size, size//3)
        pygame.draw.ellipse(screen, shadow_color,
                           (base_rect.x + 2, base_rect.y + 2, base_rect.width, base_rect.height))
        pygame.draw.ellipse(screen, piece_color, base_rect)
        
        # Main body (wider than queen)
        body_rect = pygame.Rect(x - size//2.5, y - size//4, size//1.25, size//1.2)
        pygame.draw.ellipse(screen, shadow_color,
                           (body_rect.x + 2, body_rect.y + 2, body_rect.width, body_rect.height))
        pygame.draw.ellipse(screen, piece_color, body_rect)
        
        # Crown base
        crown_rect = pygame.Rect(x - size//3, y - size//2, size//1.5, size//4)
        pygame.draw.rect(screen, shadow_color,
                        (crown_rect.x + 2, crown_rect.y + 2, crown_rect.width, crown_rect.height))
        pygame.draw.rect(screen, piece_color, crown_rect)
        
        # Crown details
        for i in range(3):
            detail_x = x - size//4 + i * size//4
            pygame.draw.line(screen, GOLD if color == PieceColor.WHITE else SILVER,
                           (detail_x, y - size//2), (detail_x, y - size//3), 2)
        
        # Cross on top of crown
        cross_size = size // 6
        cross_y = y - size//2 - cross_size
        
        # Cross shadow
        pygame.draw.line(screen, shadow_color,
                        (x - cross_size + 2, cross_y + 2), (x + cross_size + 2, cross_y + 2), 4)
        pygame.draw.line(screen, shadow_color,
                        (x + 2, cross_y - cross_size//2 + 2), (x + 2, cross_y + cross_size//2 + 2), 4)
        
        # Main cross
        pygame.draw.line(screen, GOLD if color == PieceColor.WHITE else SILVER,
                        (x - cross_size, cross_y), (x + cross_size, cross_y), 3)
        pygame.draw.line(screen, GOLD if color == PieceColor.WHITE else SILVER,
                        (x, cross_y - cross_size//2), (x, cross_y + cross_size//2), 3)
        
        # Royal orb
        pygame.draw.circle(screen, RED, (x, y - size//6), 4)
        pygame.draw.circle(screen, WHITE, (x - 1, y - size//6 - 1), 1)

class EnhancedChessGame:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("♔ Royal Chess - Ultimate Edition with Graphics")
        self.clock = pygame.time.Clock()
        
        # Enhanced fonts
        self.font_title = pygame.font.Font(None, 64)
        self.font_large = pygame.font.Font(None, 48)
        self.font_medium = pygame.font.Font(None, 36)
        self.font_small = pygame.font.Font(None, 28)
        self.font_tiny = pygame.font.Font(None, 20)
        
        # Game state
        self.board = [[None for _ in range(8)] for _ in range(8)]
        self.current_player = PieceColor.WHITE
        self.selected_piece = None
        self.selected_pos = None
        self.valid_moves = []
        self.game_mode = GameMode.MENU
        self.winner = None
        self.move_history = []
        
        # AI settings
        self.ai_color = PieceColor.BLACK
        self.player_color = PieceColor.WHITE
        self.ai_difficulty = Difficulty.MEDIUM
        
        # Animation system
        self.animated_pieces = []
        self.is_animating = False
        
        # Visual effects
        self.particles = []
        self.last_move = None
        self.captured_pieces = {"white": [], "black": []}
        
        # Menu state
        self.menu_selection = 0
        self.difficulty_selection = 1
        
        # Initialize board
        self.setup_board()
        
        # Sound system
        self.sound_enabled = True
        self.init_sounds()
        
    def init_sounds(self):
        """Initialize enhanced sound effects"""
        try:
            pygame.mixer.init(frequency=44100, size=-16, channels=2, buffer=1024)
            self.sound_enabled = True
        except:
            self.sound_enabled = False
            
    def play_sound(self, sound_type):
        """Play enhanced sound effects"""
        if not self.sound_enabled:
            return
            
        try:
            if sound_type == "move":
                duration = 0.15
                sample_rate = 44100
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 300 + 100 * math.sin(i * 0.001)
                    wave = 800 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "capture":
                duration = 0.25
                sample_rate = 44100
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 200 + (i * 400 / frames)
                    wave = 1200 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    noise = random.randint(-100, 100)
                    wave += noise
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "check":
                duration = 0.6
                sample_rate = 44100
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 440 + 220 * math.sin(i * 0.002)
                    wave = 1000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "checkmate":
                duration = 1.5
                sample_rate = 44100
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq1 = 261.63  # C4
                    freq2 = 329.63  # E4
                    freq3 = 392.00  # G4
                    
                    wave1 = 600 * math.sin(2 * math.pi * freq1 * i / sample_rate)
                    wave2 = 600 * math.sin(2 * math.pi * freq2 * i / sample_rate)
                    wave3 = 600 * math.sin(2 * math.pi * freq3 * i / sample_rate)
                    
                    wave = int((wave1 + wave2 + wave3) * (1 - i / frames))
                    arr.append([wave, wave])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "select":
                duration = 0.1
                sample_rate = 44100
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 400 * math.sin(2 * math.pi * 800 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def setup_board(self):
        """Initialize chess board with pieces"""
        # Clear board
        self.board = [[None for _ in range(8)] for _ in range(8)]
        
        # Place pawns
        for col in range(8):
            self.board[1][col] = Piece(PieceType.PAWN, PieceColor.BLACK, 1, col)
            self.board[6][col] = Piece(PieceType.PAWN, PieceColor.WHITE, 6, col)
        
        # Place other pieces
        piece_order = [PieceType.ROOK, PieceType.KNIGHT, PieceType.BISHOP, PieceType.QUEEN,
                      PieceType.KING, PieceType.BISHOP, PieceType.KNIGHT, PieceType.ROOK]
        
        for col in range(8):
            self.board[0][col] = Piece(piece_order[col], PieceColor.BLACK, 0, col)
            self.board[7][col] = Piece(piece_order[col], PieceColor.WHITE, 7, col)
        
        # Reset captured pieces
        self.captured_pieces = {"white": [], "black": []}
    
    def is_valid_position(self, row, col):
        """Check if position is within board boundaries"""
        return 0 <= row < 8 and 0 <= col < 8
    
    def get_valid_moves(self, piece, row, col):
        """Get all valid moves for a piece"""
        if not piece:
            return []
        
        moves = []
        
        if piece.type == PieceType.PAWN:
            moves = self.get_pawn_moves(piece, row, col)
        elif piece.type == PieceType.ROOK:
            moves = self.get_rook_moves(piece, row, col)
        elif piece.type == PieceType.BISHOP:
            moves = self.get_bishop_moves(piece, row, col)
        elif piece.type == PieceType.QUEEN:
            moves = self.get_queen_moves(piece, row, col)
        elif piece.type == PieceType.KING:
            moves = self.get_king_moves(piece, row, col)
        elif piece.type == PieceType.KNIGHT:
            moves = self.get_knight_moves(piece, row, col)
        
        # Filter out moves that would put own king in check
        valid_moves = []
        for move_row, move_col in moves:
            if self.is_legal_move(piece, row, col, move_row, move_col):
                valid_moves.append((move_row, move_col))
        
        return valid_moves
    
    def get_pawn_moves(self, piece, row, col):
        """Get valid moves for pawn"""
        moves = []
        direction = -1 if piece.color == PieceColor.WHITE else 1
        
        # Move forward one square
        new_row = row + direction
        if self.is_valid_position(new_row, col) and self.board[new_row][col] is None:
            moves.append((new_row, col))
            
            # Move forward two squares from starting position
            if not piece.has_moved:
                new_row = row + 2 * direction
                if self.is_valid_position(new_row, col) and self.board[new_row][col] is None:
                    moves.append((new_row, col))
        
        # Capture diagonally
        for dc in [-1, 1]:
            new_row = row + direction
            new_col = col + dc
            if self.is_valid_position(new_row, new_col):
                target = self.board[new_row][new_col]
                if target and target.color != piece.color:
                    moves.append((new_row, new_col))
        
        return moves
    
    def get_rook_moves(self, piece, row, col):
        """Get valid moves for rook"""
        moves = []
        directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        
        for dr, dc in directions:
            for i in range(1, 8):
                new_row = row + i * dr
                new_col = col + i * dc
                
                if not self.is_valid_position(new_row, new_col):
                    break
                
                target = self.board[new_row][new_col]
                if target is None:
                    moves.append((new_row, new_col))
                elif target.color != piece.color:
                    moves.append((new_row, new_col))
                    break
                else:
                    break
        
        return moves
    
    def get_bishop_moves(self, piece, row, col):
        """Get valid moves for bishop"""
        moves = []
        directions = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
        
        for dr, dc in directions:
            for i in range(1, 8):
                new_row = row + i * dr
                new_col = col + i * dc
                
                if not self.is_valid_position(new_row, new_col):
                    break
                
                target = self.board[new_row][new_col]
                if target is None:
                    moves.append((new_row, new_col))
                elif target.color != piece.color:
                    moves.append((new_row, new_col))
                    break
                else:
                    break
        
        return moves
    
    def get_queen_moves(self, piece, row, col):
        """Get valid moves for queen"""
        return self.get_rook_moves(piece, row, col) + self.get_bishop_moves(piece, row, col)
    
    def get_king_moves(self, piece, row, col):
        """Get valid moves for king"""
        moves = []
        directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
        
        for dr, dc in directions:
            new_row = row + dr
            new_col = col + dc
            
            if self.is_valid_position(new_row, new_col):
                target = self.board[new_row][new_col]
                if target is None or target.color != piece.color:
                    moves.append((new_row, new_col))
        
        return moves
    
    def get_knight_moves(self, piece, row, col):
        """Get valid moves for knight"""
        moves = []
        knight_moves = [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]
        
        for dr, dc in knight_moves:
            new_row = row + dr
            new_col = col + dc
            
            if self.is_valid_position(new_row, new_col):
                target = self.board[new_row][new_col]
                if target is None or target.color != piece.color:
                    moves.append((new_row, new_col))
        
        return moves
    
    def is_legal_move(self, piece, from_row, from_col, to_row, to_col):
        """Check if move is legal (doesn't put own king in check)"""
        # Make temporary move
        original_piece = self.board[to_row][to_col]
        self.board[to_row][to_col] = piece
        self.board[from_row][from_col] = None
        
        # Check if king is in check after this move
        king_safe = not self.is_king_in_check(piece.color)
        
        # Restore board
        self.board[from_row][from_col] = piece
        self.board[to_row][to_col] = original_piece
        
        return king_safe
    
    def find_king(self, color):
        """Find the king of the given color"""
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.type == PieceType.KING and piece.color == color:
                    return row, col
        return None
    
    def is_king_in_check(self, color):
        """Check if the king of the given color is in check"""
        king_pos = self.find_king(color)
        if not king_pos:
            return False
        
        king_row, king_col = king_pos
        
        # Check if any enemy piece can attack the king
        enemy_color = PieceColor.BLACK if color == PieceColor.WHITE else PieceColor.WHITE
        
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.color == enemy_color:
                    moves = self.get_raw_moves(piece, row, col)
                    if (king_row, king_col) in moves:
                        return True
        
        return False
    
    def get_raw_moves(self, piece, row, col):
        """Get moves without checking for king safety"""
        if piece.type == PieceType.PAWN:
            return self.get_pawn_moves(piece, row, col)
        elif piece.type == PieceType.ROOK:
            return self.get_rook_moves(piece, row, col)
        elif piece.type == PieceType.BISHOP:
            return self.get_bishop_moves(piece, row, col)
        elif piece.type == PieceType.QUEEN:
            return self.get_queen_moves(piece, row, col)
        elif piece.type == PieceType.KING:
            return self.get_king_moves(piece, row, col)
        elif piece.type == PieceType.KNIGHT:
            return self.get_knight_moves(piece, row, col)
        return []
    
    def is_checkmate(self, color):
        """Check if the given color is in checkmate"""
        if not self.is_king_in_check(color):
            return False
        
        # Try all possible moves to see if any can get out of check
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.color == color:
                    moves = self.get_valid_moves(piece, row, col)
                    if moves:
                        return False
        
        return True
    
    def is_stalemate(self, color):
        """Check if the given color is in stalemate"""
        if self.is_king_in_check(color):
            return False
        
        # Check if there are any valid moves
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.color == color:
                    moves = self.get_valid_moves(piece, row, col)
                    if moves:
                        return False
        
        return True
    
    def create_move_animation(self, from_row, from_col, to_row, to_col):
        """Create smooth animation for piece movement"""
        piece = self.board[from_row][from_col]
        
        start_x = BOARD_OFFSET_X + from_col * CELL_SIZE + CELL_SIZE // 2
        start_y = BOARD_OFFSET_Y + from_row * CELL_SIZE + CELL_SIZE // 2
        end_x = BOARD_OFFSET_X + to_col * CELL_SIZE + CELL_SIZE // 2
        end_y = BOARD_OFFSET_Y + to_row * CELL_SIZE + CELL_SIZE // 2
        
        animated_piece = AnimatedPiece(piece, (start_x, start_y), (end_x, end_y))
        self.animated_pieces.append(animated_piece)
        self.is_animating = True
    
    def make_move(self, from_row, from_col, to_row, to_col, animate=True):
        """Make a move on the board with animation"""
        piece = self.board[from_row][from_col]
        captured_piece = self.board[to_row][to_col]
        
        # Create animation if requested
        if animate and not self.is_animating:
            self.create_move_animation(from_row, from_col, to_row, to_col)
        
        # Move piece
        self.board[to_row][to_col] = piece
        self.board[from_row][from_col] = None
        
        # Update piece position and mark as moved
        piece.row = to_row
        piece.col = to_col
        piece.has_moved = True
        
        # Handle captured piece
        if captured_piece:
            color_key = "white" if captured_piece.color == PieceColor.WHITE else "black"
            self.captured_pieces[color_key].append(captured_piece)
            self.play_sound("capture")
            self.create_capture_particles(to_row, to_col, captured_piece.color)
        else:
            self.play_sound("move")
        
        # Record move
        self.last_move = ((from_row, from_col), (to_row, to_col))
        move_notation = self.get_move_notation(piece, from_row, from_col, to_row, to_col, captured_piece)
        self.move_history.append(move_notation)
        
        # Check for pawn promotion
        if piece.type == PieceType.PAWN:
            if (piece.color == PieceColor.WHITE and to_row == 0) or \
               (piece.color == PieceColor.BLACK and to_row == 7):
                piece.type = PieceType.QUEEN
                self.create_promotion_particles(to_row, to_col)
        
        # Switch turns
        self.current_player = PieceColor.BLACK if self.current_player == PieceColor.WHITE else PieceColor.WHITE
        
        # Check for check/checkmate
        if self.is_king_in_check(self.current_player):
            if self.is_checkmate(self.current_player):
                self.winner = PieceColor.BLACK if self.current_player == PieceColor.WHITE else PieceColor.WHITE
                self.game_mode = GameMode.GAME_OVER
                self.play_sound("checkmate")
            else:
                self.play_sound("check")
        elif self.is_stalemate(self.current_player):
            self.winner = "stalemate"
            self.game_mode = GameMode.GAME_OVER
    
    def get_move_notation(self, piece, from_row, from_col, to_row, to_col, captured_piece):
        """Get algebraic notation for move"""
        piece_symbols = {
            PieceType.KING: "K", PieceType.QUEEN: "Q", PieceType.ROOK: "R",
            PieceType.BISHOP: "B", PieceType.KNIGHT: "N", PieceType.PAWN: ""
        }
        
        piece_symbol = piece_symbols[piece.type]
        from_square = chr(ord('a') + from_col) + str(8 - from_row)
        to_square = chr(ord('a') + to_col) + str(8 - to_row)
        
        capture_symbol = "x" if captured_piece else ""
        
        return f"{piece_symbol}{from_square}{capture_symbol}{to_square}"
    
    def create_capture_particles(self, row, col, color):
        """Create enhanced particles when a piece is captured"""
        x = BOARD_OFFSET_X + col * CELL_SIZE + CELL_SIZE // 2
        y = BOARD_OFFSET_Y + row * CELL_SIZE + CELL_SIZE // 2
        
        particle_color = GOLD if color == PieceColor.WHITE else SILVER
        
        for _ in range(20):
            velocity = (random.uniform(-8, 8), random.uniform(-12, -3))
            self.particles.append(Particle(x, y, particle_color, velocity, life=80))
    
    def create_promotion_particles(self, row, col):
        """Create special particles for pawn promotion"""
        x = BOARD_OFFSET_X + col * CELL_SIZE + CELL_SIZE // 2
        y = BOARD_OFFSET_Y + row * CELL_SIZE + CELL_SIZE // 2
        
        for _ in range(30):
            velocity = (random.uniform(-6, 6), random.uniform(-15, -5))
            color = random.choice([GOLD, PURPLE, BLUE])
            self.particles.append(Particle(x, y, color, velocity, life=120))
    
    def get_ai_move(self):
        """Get AI move based on difficulty"""
        if self.ai_difficulty == Difficulty.EASY:
            return self.get_random_move(self.ai_color)
        else:
            depth = self.ai_difficulty.value[1]
            _, best_move = self.minimax(depth, self.ai_color, float('-inf'), float('inf'), True)
            return best_move
    
    def get_random_move(self, color):
        """Get a random valid move for the given color"""
        valid_moves = []
        
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.color == color:
                    moves = self.get_valid_moves(piece, row, col)
                    for move_row, move_col in moves:
                        valid_moves.append(((row, col), (move_row, move_col)))
        
        return random.choice(valid_moves) if valid_moves else None
    
    def minimax(self, depth, color, alpha, beta, maximizing_player):
        """Enhanced minimax algorithm with alpha-beta pruning"""
        if depth == 0 or self.is_checkmate(color) or self.is_stalemate(color):
            return self.evaluate_position(color), None
        
        moves = self.get_all_valid_moves(color)
        if not moves:
            return self.evaluate_position(color), None
        
        # Sort moves for better pruning (captures first)
        moves.sort(key=lambda move: self.move_priority(move), reverse=True)
        
        best_move = None
        
        if maximizing_player:
            max_eval = float('-inf')
            for move in moves:
                from_pos, to_pos = move
                from_row, from_col = from_pos
                to_row, to_col = to_pos
                
                # Make move
                captured_piece = self.board[to_row][to_col]
                piece = self.board[from_row][from_col]
                self.board[to_row][to_col] = piece
                self.board[from_row][from_col] = None
                
                # Evaluate
                next_color = PieceColor.BLACK if color == PieceColor.WHITE else PieceColor.WHITE
                eval_score, _ = self.minimax(depth - 1, next_color, alpha, beta, False)
                
                # Undo move
                self.board[from_row][from_col] = piece
                self.board[to_row][to_col] = captured_piece
                
                if eval_score > max_eval:
                    max_eval = eval_score
                    best_move = move
                
                alpha = max(alpha, eval_score)
                if beta <= alpha:
                    break
            
            return max_eval, best_move
        else:
            min_eval = float('inf')
            for move in moves:
                from_pos, to_pos = move
                from_row, from_col = from_pos
                to_row, to_col = to_pos
                
                # Make move
                captured_piece = self.board[to_row][to_col]
                piece = self.board[from_row][from_col]
                self.board[to_row][to_col] = piece
                self.board[from_row][from_col] = None
                
                # Evaluate
                next_color = PieceColor.BLACK if color == PieceColor.WHITE else PieceColor.WHITE
                eval_score, _ = self.minimax(depth - 1, next_color, alpha, beta, True)
                
                # Undo move
                self.board[from_row][from_col] = piece
                self.board[to_row][to_col] = captured_piece
                
                if eval_score < min_eval:
                    min_eval = eval_score
                    best_move = move
                
                beta = min(beta, eval_score)
                if beta <= alpha:
                    break
            
            return min_eval, best_move
    
    def move_priority(self, move):
        """Calculate move priority for better move ordering"""
        from_pos, to_pos = move
        to_row, to_col = to_pos
        captured_piece = self.board[to_row][to_col]
        
        priority = 0
        if captured_piece:
            priority += captured_piece.value
        
        return priority
    
    def get_all_valid_moves(self, color):
        """Get all valid moves for a color"""
        moves = []
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece and piece.color == color:
                    valid_moves = self.get_valid_moves(piece, row, col)
                    for move_row, move_col in valid_moves:
                        moves.append(((row, col), (move_row, move_col)))
        return moves
    
    def evaluate_position(self, color):
        """Enhanced position evaluation"""
        piece_values = {
            PieceType.PAWN: 100,
            PieceType.KNIGHT: 300,
            PieceType.BISHOP: 320,
            PieceType.ROOK: 500,
            PieceType.QUEEN: 900,
            PieceType.KING: 10000
        }
        
        score = 0
        
        for row in range(8):
            for col in range(8):
                piece = self.board[row][col]
                if piece:
                    value = piece_values[piece.type]
                    
                    # Add positional bonuses
                    if piece.type == PieceType.PAWN:
                        if piece.color == PieceColor.WHITE:
                            value += (7 - row) * 10
                        else:
                            value += row * 10
                    
                    elif piece.type == PieceType.KNIGHT:
                        center_distance = abs(3.5 - row) + abs(3.5 - col)
                        value += (7 - center_distance) * 5
                    
                    if piece.color == color:
                        score += value
                    else:
                        score -= value
        
        return score
    
    def handle_click(self, pos):
        """Handle mouse click on board"""
        x, y = pos
        
        # Check if click is on board
        if (BOARD_OFFSET_X <= x < BOARD_OFFSET_X + BOARD_SIZE and 
            BOARD_OFFSET_Y <= y < BOARD_OFFSET_Y + BOARD_SIZE):
            
            if self.is_animating:
                return
            
            col = (x - BOARD_OFFSET_X) // CELL_SIZE
            row = (y - BOARD_OFFSET_Y) // CELL_SIZE
            
            if self.selected_piece is None:
                # Select piece
                piece = self.board[row][col]
                if piece and piece.color == self.current_player:
                    # In AI mode, only allow player to move their pieces
                    if (self.game_mode == GameMode.HUMAN_VS_AI and 
                        piece.color != self.player_color):
                        return
                    
                    self.selected_piece = piece
                    self.selected_pos = (row, col)
                    self.valid_moves = self.get_valid_moves(piece, row, col)
                    self.play_sound("select")
            else:
                # Make move or select new piece
                if (row, col) in self.valid_moves:
                    # Make move
                    from_row, from_col = self.selected_pos
                    self.make_move(from_row, from_col, row, col)
                    
                    self.selected_piece = None
                    self.selected_pos = None
                    self.valid_moves = []
                else:
                    # Select new piece or deselect
                    piece = self.board[row][col]
                    if piece and piece.color == self.current_player:
                        # In AI mode, only allow player to move their pieces
                        if (self.game_mode == GameMode.HUMAN_VS_AI and 
                            piece.color != self.player_color):
                            return
                        
                        self.selected_piece = piece
                        self.selected_pos = (row, col)
                        self.valid_moves = self.get_valid_moves(piece, row, col)
                        self.play_sound("select")
                    else:
                        self.selected_piece = None
                        self.selected_pos = None
                        self.valid_moves = []
    
    def update_animations(self):
        """Update piece animations"""
        if self.animated_pieces:
            completed_animations = []
            for i, animated_piece in enumerate(self.animated_pieces):
                if animated_piece.update():
                    completed_animations.append(i)
            
            # Remove completed animations
            for i in reversed(completed_animations):
                self.animated_pieces.pop(i)
            
            # Check if all animations are done
            if not self.animated_pieces:
                self.is_animating = False
    
    def update_particles(self):
        """Update particle effects"""
        self.particles = [p for p in self.particles if p.life > 0]
        for particle in self.particles:
            particle.update()
    
    def draw_enhanced_board(self):
        """Draw beautiful chess board with enhanced visuals"""
        # Board shadow
        shadow_rect = pygame.Rect(BOARD_OFFSET_X + 5, BOARD_OFFSET_Y + 5, BOARD_SIZE, BOARD_SIZE)
        pygame.draw.rect(self.screen, DARK_GRAY, shadow_rect)
        
        # Board border
        border_rect = pygame.Rect(BOARD_OFFSET_X - 10, BOARD_OFFSET_Y - 10, BOARD_SIZE + 20, BOARD_SIZE + 20)
        pygame.draw.rect(self.screen, DARK_BROWN, border_rect)
        pygame.draw.rect(self.screen, GOLD, border_rect, 3)
        
        # Draw squares
        for row in range(8):
            for col in range(8):
                # Square colors
                is_light = (row + col) % 2 == 0
                color = CREAM if is_light else BROWN
                
                # Highlight effects
                highlight = None
                if self.selected_pos == (row, col):
                    highlight = GOLD
                elif (row, col) in self.valid_moves:
                    highlight = LIGHT_GREEN
                elif (self.last_move and 
                      ((row, col) == self.last_move[0] or (row, col) == self.last_move[1])):
                    highlight = BLUE
                
                # Draw square
                rect = pygame.Rect(BOARD_OFFSET_X + col * CELL_SIZE, 
                                 BOARD_OFFSET_Y + row * CELL_SIZE, 
                                 CELL_SIZE, CELL_SIZE)
                
                pygame.draw.rect(self.screen, color, rect)
                
                # Draw highlight
                if highlight:
                    pygame.draw.rect(self.screen, highlight, rect, 4)
                
                # Add subtle square borders for 3D effect
                pygame.draw.line(self.screen, WHITE, 
                               (rect.left, rect.top), (rect.right, rect.top), 1)
                pygame.draw.line(self.screen, WHITE,
                               (rect.left, rect.top), (rect.left, rect.bottom), 1)
                pygame.draw.line(self.screen, DARK_GRAY,
                               (rect.right - 1, rect.top), (rect.right - 1, rect.bottom), 1)
                pygame.draw.line(self.screen, DARK_GRAY,
                               (rect.left, rect.bottom - 1), (rect.right, rect.bottom - 1), 1)
                
                # Draw piece (if not being animated)
                piece = self.board[row][col]
                if piece and not self.is_piece_being_animated(piece):
                    self.draw_piece_graphics(piece, row, col)
        
        # Draw animated pieces
        for animated_piece in self.animated_pieces:
            self.draw_animated_piece_graphics(animated_piece)
        
        # Draw valid move indicators
        for row, col in self.valid_moves:
            center_x = BOARD_OFFSET_X + col * CELL_SIZE + CELL_SIZE // 2
            center_y = BOARD_OFFSET_Y + row * CELL_SIZE + CELL_SIZE // 2
            
            if self.board[row][col] is None:  # Empty square
                pygame.draw.circle(self.screen, LIGHT_GREEN, (center_x, center_y), 8)
                pygame.draw.circle(self.screen, DARK_GREEN, (center_x, center_y), 8, 2)
            else:  # Capture square
                pygame.draw.circle(self.screen, RED, (center_x, center_y), 12, 3)
        
        # Draw coordinates
        self.draw_coordinates()
    
    def is_piece_being_animated(self, piece):
        """Check if piece is currently being animated"""
        for animated_piece in self.animated_pieces:
            if animated_piece.piece == piece:
                return True
        return False
    
    def draw_piece_graphics(self, piece, row, col):
        """Draw piece using custom graphics"""
        x = BOARD_OFFSET_X + col * CELL_SIZE + CELL_SIZE // 2
        y = BOARD_OFFSET_Y + row * CELL_SIZE + CELL_SIZE // 2
        
        size = CELL_SIZE // 3
        
        # Selection glow
        if self.selected_piece == piece:
            glow_radius = int(size * 1.5 + 5 * math.sin(time.time() * 8))
            pygame.draw.circle(self.screen, GOLD, (x, y), glow_radius, 3)
        
        # Draw the piece using our custom renderer
        if piece.type == PieceType.PAWN:
            PieceRenderer.draw_pawn(self.screen, x, y, piece.color, size)
        elif piece.type == PieceType.ROOK:
            PieceRenderer.draw_rook(self.screen, x, y, piece.color, size)
        elif piece.type == PieceType.KNIGHT:
            PieceRenderer.draw_knight(self.screen, x, y, piece.color, size)
        elif piece.type == PieceType.BISHOP:
            PieceRenderer.draw_bishop(self.screen, x, y, piece.color, size)
        elif piece.type == PieceType.QUEEN:
            PieceRenderer.draw_queen(self.screen, x, y, piece.color, size)
        elif piece.type == PieceType.KING:
            PieceRenderer.draw_king(self.screen, x, y, piece.color, size)
    
    def draw_animated_piece_graphics(self, animated_piece):
        """Draw piece during animation with enhanced effects"""
        piece = animated_piece.piece
        x = int(animated_piece.current_x)
        y = int(animated_piece.current_y)
        
        size = int(CELL_SIZE // 3 * (1.0 + 0.2 * math.sin(animated_piece.progress * math.pi)))
        
        # Animation glow trail
        trail_alpha = int(100 * (1 - animated_piece.progress))
        if trail_alpha > 0:
            trail_surface = pygame.Surface((size * 3, size * 3), pygame.SRCALPHA)
            pygame.draw.circle(trail_surface, (*GOLD, trail_alpha), (size * 1.5, size * 1.5), size)
            self.screen.blit(trail_surface, (x - size * 1.5, y - size * 1.5))
        
        # Draw the animated piece
        if piece.type == PieceType.PAWN:
            PieceRenderer.draw_pawn(self.screen, x, y, piece.color, size, animated_piece.rotation)
        elif piece.type == PieceType.ROOK:
            PieceRenderer.draw_rook(self.screen, x, y, piece.color, size, animated_piece.rotation)
        elif piece.type == PieceType.KNIGHT:
            PieceRenderer.draw_knight(self.screen, x, y, piece.color, size, animated_piece.rotation)
        elif piece.type == PieceType.BISHOP:
            PieceRenderer.draw_bishop(self.screen, x, y, piece.color, size, animated_piece.rotation)
        elif piece.type == PieceType.QUEEN:
            PieceRenderer.draw_queen(self.screen, x, y, piece.color, size, animated_piece.rotation)
        elif piece.type == PieceType.KING:
            PieceRenderer.draw_king(self.screen, x, y, piece.color, size, animated_piece.rotation)
    
    def draw_coordinates(self):
        """Draw board coordinates"""
        font = self.font_small
        
        # Files (a-h)
        for col in range(8):
            letter = chr(ord('a') + col)
            text = font.render(letter, True, GOLD)
            text_rect = text.get_rect(center=(
                BOARD_OFFSET_X + col * CELL_SIZE + CELL_SIZE // 2,
                BOARD_OFFSET_Y + BOARD_SIZE + 15
            ))
            self.screen.blit(text, text_rect)
        
        # Ranks (1-8)
        for row in range(8):
            number = str(8 - row)
            text = font.render(number, True, GOLD)
            text_rect = text.get_rect(center=(
                BOARD_OFFSET_X - 20,
                BOARD_OFFSET_Y + row * CELL_SIZE + CELL_SIZE // 2
            ))
            self.screen.blit(text, text_rect)
    
    def draw_enhanced_ui(self):
        """Draw enhanced game UI"""
        ui_x = BOARD_OFFSET_X + BOARD_SIZE + 30
        
        # Game title
        title = self.font_large.render("♔ ROYAL CHESS ♔", True, GOLD)
        self.screen.blit(title, (ui_x, 20))
        
        # Current player with styling
        player_name = "White" if self.current_player == PieceColor.WHITE else "Black"
        player_text = f"Turn: {player_name}"
        player_color = PIECE_WHITE if self.current_player == PieceColor.WHITE else PIECE_BLACK
        
        # Player indicator background
        player_bg = pygame.Rect(ui_x - 5, 70, 200, 40)
        pygame.draw.rect(self.screen, LIGHT_GRAY, player_bg)
        pygame.draw.rect(self.screen, player_color, player_bg, 3)
        
        player_surface = self.font_medium.render(player_text, True, player_color)
        self.screen.blit(player_surface, (ui_x, 80))
        
        # Game mode
        mode_text = ""
        if self.game_mode == GameMode.HUMAN_VS_HUMAN:
            mode_text = "Mode: Player vs Player"
        elif self.game_mode == GameMode.HUMAN_VS_AI:
            difficulty_name = self.ai_difficulty.value[0]
            player_color_name = "White" if self.player_color == PieceColor.WHITE else "Black"
            mode_text = f"Mode: vs AI ({difficulty_name})"
            mode_text2 = f"You play as: {player_color_name}"
            
            mode_surface2 = self.font_small.render(mode_text2, True, DARK_GRAY)
            self.screen.blit(mode_surface2, (ui_x, 140))
        
        mode_surface = self.font_small.render(mode_text, True, DARK_GRAY)
        self.screen.blit(mode_surface, (ui_x, 120))
        
        # Check status
        if self.is_king_in_check(self.current_player):
            check_text = f"{player_name} King in Check!"
            check_bg = pygame.Rect(ui_x - 5, 160, 250, 30)
            pygame.draw.rect(self.screen, RED, check_bg)
            check_surface = self.font_medium.render(check_text, True, WHITE)
            self.screen.blit(check_surface, (ui_x, 165))
        
        # Captured pieces with graphics
        self.draw_captured_pieces_graphics(ui_x)
        
        # Move history
        self.draw_move_history(ui_x)
        
        # Controls
        self.draw_controls(ui_x)
    
    def draw_captured_pieces_graphics(self, ui_x):
        """Draw captured pieces with small graphics"""
        y_offset = 200
        
        # White captured pieces
        white_title = self.font_small.render("Captured (White):", True, BLACK)
        self.screen.blit(white_title, (ui_x, y_offset))
        
        x_offset = 0
        y_current = y_offset + 25
        for piece in self.captured_pieces["white"]:
            # Draw small version of piece
            piece_x = ui_x + x_offset
            piece_y = y_current
            size = 15
            
            if piece.type == PieceType.PAWN:
                PieceRenderer.draw_pawn(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.ROOK:
                PieceRenderer.draw_rook(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.KNIGHT:
                PieceRenderer.draw_knight(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.BISHOP:
                PieceRenderer.draw_bishop(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.QUEEN:
                PieceRenderer.draw_queen(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.KING:
                PieceRenderer.draw_king(self.screen, piece_x, piece_y, piece.color, size)
            
            x_offset += 35
            if x_offset > 200:  # Wrap to next line
                x_offset = 0
                y_current += 35
        
        y_offset = max(y_current + 20, y_offset + 80)
        
        # Black captured pieces
        black_title = self.font_small.render("Captured (Black):", True, BLACK)
        self.screen.blit(black_title, (ui_x, y_offset))
        
        x_offset = 0
        y_current = y_offset + 25
        for piece in self.captured_pieces["black"]:
            piece_x = ui_x + x_offset
            piece_y = y_current
            size = 15
            
            if piece.type == PieceType.PAWN:
                PieceRenderer.draw_pawn(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.ROOK:
                PieceRenderer.draw_rook(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.KNIGHT:
                PieceRenderer.draw_knight(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.BISHOP:
                PieceRenderer.draw_bishop(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.QUEEN:
                PieceRenderer.draw_queen(self.screen, piece_x, piece_y, piece.color, size)
            elif piece.type == PieceType.KING:
                PieceRenderer.draw_king(self.screen, piece_x, piece_y, piece.color, size)
            
            x_offset += 35
            if x_offset > 200:
                x_offset = 0
                y_current += 35
    
    def draw_move_history(self, ui_x):
        """Draw move history with better formatting"""
        y_start = 400
        
        history_title = self.font_medium.render("Move History", True, DARK_BROWN)
        self.screen.blit(history_title, (ui_x, y_start))
        
        # Show last 8 moves
        recent_moves = self.move_history[-8:]
        for i, move in enumerate(recent_moves):
            move_num = len(self.move_history) - len(recent_moves) + i + 1
            move_text = f"{move_num}. {move}"
            
            color = BLACK if i % 2 == 0 else DARK_GRAY
            move_surface = self.font_small.render(move_text, True, color)
            self.screen.blit(move_surface, (ui_x, y_start + 30 + i * 22))
    
    def draw_controls(self, ui_x):
        """Draw control instructions"""
        y_start = 620
        
        controls_title = self.font_small.render("Controls:", True, DARK_BROWN)
        self.screen.blit(controls_title, (ui_x, y_start))
        
        controls = [
            "Click: Select/Move pieces",
            "ESC: Back to menu",
            "R: Restart game"
        ]
        
        for i, control in enumerate(controls):
            control_surface = self.font_tiny.render(control, True, DARK_GRAY)
            self.screen.blit(control_surface, (ui_x, y_start + 25 + i * 20))
    
    def draw_menu(self):
        """Draw enhanced main menu"""
        # Gradient background
        for y in range(WINDOW_HEIGHT):
            color_ratio = y / WINDOW_HEIGHT
            r = int(240 - 80 * color_ratio)
            g = int(217 - 50 * color_ratio)
            b = int(181 - 30 * color_ratio)
            pygame.draw.line(self.screen, (r, g, b), (0, y), (WINDOW_WIDTH, y))
        
        # Animated title
        title_y = 100 + math.sin(time.time() * 2) * 10
        title_text = self.font_title.render("♔ ROYAL CHESS ♔", True, GOLD)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        
        # Title shadow
        shadow_text = self.font_title.render("♔ ROYAL CHESS ♔", True, DARK_BROWN)
        shadow_rect = shadow_text.get_rect(center=(WINDOW_WIDTH // 2 + 3, title_y + 3))
        self.screen.blit(shadow_text, shadow_rect)
        self.screen.blit(title_text, title_rect)
        
        # Subtitle
        subtitle = self.font_medium.render("Ultimate Edition with Graphics", True, DARK_BROWN)
        subtitle_rect = subtitle.get_rect(center=(WINDOW_WIDTH // 2, title_y + 70))
        self.screen.blit(subtitle, subtitle_rect)
        
        # Menu options with highlighting
        menu_options = [
            "1. Player vs Player",
            "2. Player vs Computer",
            "3. Exit Game"
        ]
        
        for i, option in enumerate(menu_options):
            y_pos = 300 + i * 80
            
            # Highlight selected option
            if i == self.menu_selection:
                highlight_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, y_pos - 10, 400, 50)
                pygame.draw.rect(self.screen, GOLD, highlight_rect, 3)
                color = DARK_BROWN
            else:
                color = BLACK
            
            option_text = self.font_large.render(option, True, color)
            option_rect = option_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos))
            self.screen.blit(option_text, option_rect)
        
        # Instructions
        instructions = [
            "Use ↑↓ arrows to navigate, ENTER to select",
            "Or click on options directly"
        ]
        
        for i, instruction in enumerate(instructions):
            inst_text = self.font_small.render(instruction, True, DARK_GRAY)
            inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 600 + i * 30))
            self.screen.blit(inst_text, inst_rect)
    
    def draw_difficulty_select(self):
        """Draw difficulty selection screen"""
        # Background
        self.screen.fill(CREAM)
        
        # Title
        title = self.font_large.render("Select Difficulty", True, DARK_BROWN)
        title_rect = title.get_rect(center=(WINDOW_WIDTH // 2, 150))
        self.screen.blit(title, title_rect)
        
        # Difficulty options
        difficulties = list(Difficulty)
        
        for i, difficulty in enumerate(difficulties):
            y_pos = 250 + i * 100
            
            # Highlight selected difficulty
            if i == self.difficulty_selection:
                highlight_rect = pygame.Rect(WINDOW_WIDTH // 2 - 250, y_pos - 20, 500, 80)
                pygame.draw.rect(self.screen, GOLD, highlight_rect, 3)
            
            # Difficulty name
            name_text = self.font_medium.render(difficulty.value[0], True, DARK_BROWN)
            name_rect = name_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos))
            self.screen.blit(name_text, name_rect)
            
            # Description
            desc_text = self.font_small.render(difficulty.value[2], True, DARK_GRAY)
            desc_rect = desc_text.get_rect(center=(WINDOW_WIDTH // 2, y_pos + 30))
            self.screen.blit(desc_text, desc_rect)
        
        # Instructions
        inst_text = self.font_small.render("↑↓ to select, ENTER to confirm, ESC to go back", True, BLACK)
        inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 650))
        self.screen.blit(inst_text, inst_rect)
    
    def draw_color_select(self):
        """Draw color selection screen"""
        self.screen.fill(CREAM)
        
        # Title
        title = self.font_large.render("Choose Your Color", True, DARK_BROWN)
        title_rect = title.get_rect(center=(WINDOW_WIDTH // 2, 200))
        self.screen.blit(title, title_rect)
        
        # Color options with piece graphics
        colors = [("Play as White", PieceColor.WHITE), ("Play as Black", PieceColor.BLACK)]
        
        for i, (text, color) in enumerate(colors):
            y_pos = 350 + i * 120
            
            # Option box
            box_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, y_pos - 40, 400, 80)
            bg_color = WHITE if color == PieceColor.WHITE else LIGHT_GRAY
            pygame.draw.rect(self.screen, bg_color, box_rect)
            pygame.draw.rect(self.screen, GOLD, box_rect, 3)
            
            # Sample pieces
            piece_y = y_pos
            for j, piece_type in enumerate([PieceType.KING, PieceType.QUEEN, PieceType.ROOK]):
                piece_x = WINDOW_WIDTH // 2 - 100 + j * 40
                size = 20
                
                if piece_type == PieceType.KING:
                    PieceRenderer.draw_king(self.screen, piece_x, piece_y, color, size)
                elif piece_type == PieceType.QUEEN:
                    PieceRenderer.draw_queen(self.screen, piece_x, piece_y, color, size)
                elif piece_type == PieceType.ROOK:
                    PieceRenderer.draw_rook(self.screen, piece_x, piece_y, color, size)
            
            # Text
            text_color = BLACK
            option_text = self.font_medium.render(text, True, text_color)
            option_rect = option_text.get_rect(center=(WINDOW_WIDTH // 2 + 80, y_pos))
            self.screen.blit(option_text, option_rect)
        
        # Instructions
        inst_text = self.font_small.render("Click to select your color", True, BLACK)
        inst_rect = inst_text.get_rect(center=(WINDOW_WIDTH // 2, 650))
        self.screen.blit(inst_text, inst_rect)
    
    def draw_game_over(self):
        """Draw enhanced game over screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(200)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Game over panel
        panel_rect = pygame.Rect(WINDOW_WIDTH // 2 - 250, WINDOW_HEIGHT // 2 - 150, 500, 300)
        pygame.draw.rect(self.screen, CREAM, panel_rect)
        pygame.draw.rect(self.screen, GOLD, panel_rect, 5)
        
        # Result text
        if self.winner == "stalemate":
            result_text = "STALEMATE!"
            result_color = ORANGE
        else:
            winner_name = "White" if self.winner == PieceColor.WHITE else "Black"
            result_text = f"{winner_name} Wins!"
            result_color = PIECE_WHITE if self.winner == PieceColor.WHITE else PIECE_BLACK
        
        # Add crown graphic for winner
        if self.winner != "stalemate":
            crown_x = WINDOW_WIDTH // 2
            crown_y = WINDOW_HEIGHT // 2 - 80
            PieceRenderer.draw_king(self.screen, crown_x, crown_y, self.winner, 40)
        
        result_surface = self.font_large.render(result_text, True, result_color)
        result_rect = result_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 30))
        self.screen.blit(result_surface, result_rect)
        
        # Move count
        move_count_text = f"Total moves: {len(self.move_history)}"
        move_surface = self.font_small.render(move_count_text, True, DARK_GRAY)
        move_rect = move_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20))
        self.screen.blit(move_surface, move_rect)
        
        # Instructions
        instructions = [
            "Press R to play again",
            "Press ESC to return to menu"
        ]
        
        for i, instruction in enumerate(instructions):
            inst_surface = self.font_medium.render(instruction, True, DARK_BROWN)
            inst_rect = inst_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 70 + i * 40))
            self.screen.blit(inst_surface, inst_rect)
    
    def draw_particles(self):
        """Draw enhanced particles"""
        for particle in self.particles:
            particle.draw(self.screen)
    
    def handle_menu_click(self, pos):
        """Handle clicks in menu"""
        x, y = pos
        
        # Check which option was clicked
        for i in range(3):
            option_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, 290 + i * 80, 400, 50)
            if option_rect.collidepoint(x, y):
                self.menu_selection = i
                if i == 0:  # Player vs Player
                    self.start_human_vs_human()
                elif i == 1:  # Player vs Computer
                    self.game_mode = GameMode.DIFFICULTY_SELECT
                elif i == 2:  # Exit
                    return False
        return True
    
    def handle_difficulty_click(self, pos):
        """Handle clicks in difficulty selection"""
        x, y = pos
        
        difficulties = list(Difficulty)
        for i in range(len(difficulties)):
            option_rect = pygame.Rect(WINDOW_WIDTH // 2 - 250, 230 + i * 100, 500, 80)
            if option_rect.collidepoint(x, y):
                self.difficulty_selection = i
                self.ai_difficulty = difficulties[i]
                self.game_mode = GameMode.COLOR_SELECT
                break
    
    def handle_color_click(self, pos):
        """Handle clicks in color selection"""
        x, y = pos
        
        for i in range(2):
            option_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, 310 + i * 120, 400, 80)
            if option_rect.collidepoint(x, y):
                if i == 0:  # Play as White
                    self.start_human_vs_ai(PieceColor.WHITE)
                else:  # Play as Black
                    self.start_human_vs_ai(PieceColor.BLACK)
                break
    
    def handle_events(self):
        """Handle game events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    if self.game_mode in [GameMode.HUMAN_VS_HUMAN, GameMode.HUMAN_VS_AI, GameMode.GAME_OVER]:
                        self.game_mode = GameMode.MENU
                    elif self.game_mode in [GameMode.DIFFICULTY_SELECT, GameMode.COLOR_SELECT]:
                        self.game_mode = GameMode.MENU
                    else:
                        return False
                
                elif event.key == pygame.K_r:
                    if self.game_mode == GameMode.GAME_OVER:
                        self.reset_game()
                    elif self.game_mode in [GameMode.HUMAN_VS_HUMAN, GameMode.HUMAN_VS_AI]:
                        self.reset_game()
                
                elif self.game_mode == GameMode.MENU:
                    if event.key == pygame.K_UP:
                        self.menu_selection = (self.menu_selection - 1) % 3
                    elif event.key == pygame.K_DOWN:
                        self.menu_selection = (self.menu_selection + 1) % 3
                    elif event.key == pygame.K_RETURN:
                        if self.menu_selection == 0:
                            self.start_human_vs_human()
                        elif self.menu_selection == 1:
                            self.game_mode = GameMode.DIFFICULTY_SELECT
                        elif self.menu_selection == 2:
                            return False
                
                elif self.game_mode == GameMode.DIFFICULTY_SELECT:
                    if event.key == pygame.K_UP:
                        self.difficulty_selection = (self.difficulty_selection - 1) % 3
                    elif event.key == pygame.K_DOWN:
                        self.difficulty_selection = (self.difficulty_selection + 1) % 3
                    elif event.key == pygame.K_RETURN:
                        difficulties = list(Difficulty)
                        self.ai_difficulty = difficulties[self.difficulty_selection]
                        self.game_mode = GameMode.COLOR_SELECT
            
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    if self.game_mode == GameMode.MENU:
                        if not self.handle_menu_click(event.pos):
                            return False
                    elif self.game_mode == GameMode.DIFFICULTY_SELECT:
                        self.handle_difficulty_click(event.pos)
                    elif self.game_mode == GameMode.COLOR_SELECT:
                        self.handle_color_click(event.pos)
                    elif self.game_mode in [GameMode.HUMAN_VS_HUMAN, GameMode.HUMAN_VS_AI]:
                        # In AI mode, only allow human player to click during their turn
                        if (self.game_mode == GameMode.HUMAN_VS_AI and 
                            self.current_player != self.player_color):
                            continue
                        
                        self.handle_click(event.pos)
        
        return True
    
    def start_human_vs_human(self):
        """Start human vs human game"""
        self.game_mode = GameMode.HUMAN_VS_HUMAN
        self.reset_game()
    
    def start_human_vs_ai(self, player_color):
        """Start human vs AI game"""
        self.game_mode = GameMode.HUMAN_VS_AI
        self.player_color = player_color
        self.ai_color = PieceColor.BLACK if player_color == PieceColor.WHITE else PieceColor.WHITE
        self.reset_game()
    
    def reset_game(self):
        """Reset game to initial state"""
        self.setup_board()
        self.current_player = PieceColor.WHITE
        self.selected_piece = None
        self.selected_pos = None
        self.valid_moves = []
        self.winner = None
        self.move_history = []
        self.last_move = None
        self.particles = []
        self.animated_pieces = []
        self.is_animating = False
    
    def update(self):
        """Update game state"""
        self.update_animations()
        self.update_particles()
        
        # AI move (only when not animating)
        if (self.game_mode == GameMode.HUMAN_VS_AI and 
            self.current_player == self.ai_color and 
            self.winner is None and
            not self.is_animating):
            
            ai_move = self.get_ai_move()
            if ai_move:
                from_pos, to_pos = ai_move
                from_row, from_col = from_pos
                to_row, to_col = to_pos
                self.make_move(from_row, from_col, to_row, to_col)
    
    def draw(self):
        """Draw everything"""
        if self.game_mode == GameMode.MENU:
            self.draw_menu()
        elif self.game_mode == GameMode.DIFFICULTY_SELECT:
            self.draw_difficulty_select()
        elif self.game_mode == GameMode.COLOR_SELECT:
            self.draw_color_select()
        else:
            # Game background
            self.screen.fill(LIGHT_GRAY)
            
            # Draw game elements
            self.draw_enhanced_board()
            self.draw_enhanced_ui()
            
            if self.game_mode == GameMode.GAME_OVER:
                self.draw_game_over()
        
        # Always draw particles on top
        self.draw_particles()
        pygame.display.flip()
    
    def run(self):
        """Main game loop"""
        running = True
        
        while running:
            running = self.handle_events()
            self.update()
            self.draw()
            self.clock.tick(60)
        
        pygame.quit()
        sys.exit()

def main():
    print("♔ Royal Chess - Ultimate Edition with Beautiful Graphics")
    print("=" * 60)
    print("Enhanced Visual Features:")
    print("• Hand-drawn chess pieces with 3D shading")
    print("• Detailed piece graphics: King, Queen, Rook, Bishop, Knight, Pawn")
    print("• Professional chess board with wood texture effects")
    print("• Smooth piece animations with rotation and scaling")
    print("• Enhanced particle effects for captures and promotions")
    print("• Glow effects and visual highlights")
    print("• 3D shadows and depth effects")
    print("• Beautiful captured pieces display")
    print("\nGameplay Features:")
    print("• 3 AI difficulty levels: Easy, Medium, Hard")
    print("• Choose to play as White or Black vs AI")
    print("• Full chess rules implementation")
    print("• Move validation and legal move highlighting")
    print("• Check and checkmate detection")
    print("• Professional chess notation")
    print("• High-quality sound effects")
    print("\nVisual Enhancements:")
    print("• Custom drawn pieces instead of Unicode symbols")
    print("• Detailed piece designs with royal styling")
    print("• Smooth animation system with easing")
    print("• Enhanced lighting and shadow effects")
    print("• Professional game interface")
    print("\nControls:")
    print("• Click pieces to select and move")
    print("• Arrow keys to navigate menus")
    print("• ENTER to select menu options")
    print("• ESC - Back to menu")
    print("• R - Restart game")
    print("\nStarting Royal Chess with Beautiful Graphics...")
    
    try:
        game = EnhancedChessGame()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()

