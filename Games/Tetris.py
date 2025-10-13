import pygame
import random
import sys
import json
import math
from enum import Enum
from copy import deepcopy

# Initialize Pygame
pygame.init()

# Constants
WINDOW_WIDTH = 800
WINDOW_HEIGHT = 600
GRID_WIDTH = 10
GRID_HEIGHT = 20
CELL_SIZE = 25
GRID_X_OFFSET = 50
GRID_Y_OFFSET = 50

# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GRAY = (128, 128, 128)
LIGHT_GRAY = (200, 200, 200)
DARK_GRAY = (64, 64, 64)
RED = (255, 0, 0)
GREEN = (0, 255, 0)
BLUE = (0, 0, 255)
YELLOW = (255, 255, 0)
ORANGE = (255, 165, 0)
PURPLE = (128, 0, 128)
CYAN = (0, 255, 255)

# Tetris piece colors
PIECE_COLORS = [
    CYAN,    # I
    BLUE,    # J
    ORANGE,  # L
    YELLOW,  # O
    GREEN,   # S
    PURPLE,  # T
    RED      # Z
]

class GameState(Enum):
    MENU = "menu"
    PLAYING = "playing"
    PAUSED = "paused"
    GAME_OVER = "game_over"

class Tetromino:
    """Tetris pieces"""
    SHAPES = [
        # I piece
        [['.....',
          '..#..',
          '..#..',
          '..#..',
          '..#..'],
         ['.....',
          '.....',
          '####.',
          '.....',
          '.....']],
        
        # J piece
        [['.....',
          '.#...',
          '.###.',
          '.....',
          '.....'],
         ['.....',
          '..##.',
          '..#..',
          '..#..',
          '.....'],
         ['.....',
          '.....',
          '.###.',
          '...#.',
          '.....'],
         ['.....',
          '..#..',
          '..#..',
          '.##..',
          '.....']],
        
        # L piece
        [['.....',
          '...#.',
          '.###.',
          '.....',
          '.....'],
         ['.....',
          '..#..',
          '..#..',
          '..##.',
          '.....'],
         ['.....',
          '.....',
          '.###.',
          '.#...',
          '.....'],
         ['.....',
          '.##..',
          '..#..',
          '..#..',
          '.....']],
        
        # O piece
        [['.....',
          '.....',
          '.##..',
          '.##..',
          '.....']],
        
        # S piece
        [['.....',
          '.....',
          '..##.',
          '.##..',
          '.....'],
         ['.....',
          '..#..',
          '..##.',
          '...#.',
          '.....']],
        
        # T piece
        [['.....',
          '.....',
          '.###.',
          '..#..',
          '.....'],
         ['.....',
          '..#..',
          '.##..',
          '..#..',
          '.....'],
         ['.....',
          '..#..',
          '.###.',
          '.....',
          '.....'],
         ['.....',
          '..#..',
          '..##.',
          '..#..',
          '.....']],
        
        # Z piece
        [['.....',
          '.....',
          '.##..',
          '..##.',
          '.....'],
         ['.....',
          '...#.',
          '..##.',
          '..#..',
          '.....']]
    ]

class Piece:
    def __init__(self, x, y, shape):
        self.x = x
        self.y = y
        self.shape = shape
        self.color = PIECE_COLORS[shape]
        self.rotation = 0
        
    def get_rotated_shape(self, rotation=None):
        if rotation is None:
            rotation = self.rotation
        return Tetromino.SHAPES[self.shape][rotation % len(Tetromino.SHAPES[self.shape])]
    
    def get_cells(self, rotation=None, x=None, y=None):
        if x is None:
            x = self.x
        if y is None:
            y = self.y
            
        shape = self.get_rotated_shape(rotation)
        cells = []
        
        for row, line in enumerate(shape):
            for col, cell in enumerate(line):
                if cell == '#':
                    cells.append((x + col - 2, y + row - 2))  # Center the piece
        
        return cells

class Particle:
    def __init__(self, x, y, color, velocity):
        self.x = x
        self.y = y
        self.color = color
        self.velocity = velocity
        self.life = 60
        self.max_life = 60
        self.size = random.randint(2, 4)
        
    def update(self):
        self.x += self.velocity[0]
        self.y += self.velocity[1]
        self.velocity = (self.velocity[0] * 0.98, self.velocity[1] + 0.1)
        self.life -= 1
        
    def draw(self, screen):
        if self.life > 0:
            alpha = int(255 * (self.life / self.max_life))
            size = max(1, int(self.size * (self.life / self.max_life)))
            
            # Create surface with alpha for particle effect
            particle_surface = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            color_with_alpha = (*self.color, alpha)
            pygame.draw.circle(particle_surface, color_with_alpha, (size, size), size)
            screen.blit(particle_surface, (self.x - size, self.y - size))

class TetrisGame:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🧩 Tetris - Ultimate Edition")
        self.clock = pygame.time.Clock()
        
        # Fonts
        self.font_large = pygame.font.Font(None, 48)
        self.font_medium = pygame.font.Font(None, 36)
        self.font_small = pygame.font.Font(None, 24)
        
        # Game state
        self.grid = [[0 for _ in range(GRID_WIDTH)] for _ in range(GRID_HEIGHT)]
        self.current_piece = None
        self.next_piece = None
        self.held_piece = None
        self.can_hold = True
        self.fall_time = 0
        self.fall_speed = 500  # milliseconds
        
        # Score and level
        self.score = 0
        self.level = 1
        self.lines_cleared = 0
        self.high_score = self.load_high_score()
        
        # Game state
        self.game_state = GameState.MENU
        self.particles = []
        
        # Timing
        self.last_fall = pygame.time.get_ticks()
        self.key_repeat = {
            'left': 0,
            'right': 0,
            'down': 0
        }
        
        # Sound
        self.sound_enabled = True
        self.init_sounds()
        
        # Generate first pieces
        self.spawn_piece()
        
    def init_sounds(self):
        """Initialize sound effects"""
        try:
            pygame.mixer.init(frequency=22050, size=-16, channels=2, buffer=512)
            self.sound_enabled = True
        except:
            self.sound_enabled = False
    
    def play_sound(self, sound_type):
        """Play sound effects"""
        if not self.sound_enabled:
            return
            
        try:
            if sound_type == "line_clear":
                # Line clear sound
                duration = 0.3
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 523 + (i * 200 / frames)
                    wave = 2000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "tetris":
                # Tetris (4 lines) sound
                duration = 0.8
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq1 = 523 + (i * 300 / frames)
                    freq2 = 659 + (i * 300 / frames)
                    wave1 = 1500 * math.sin(2 * math.pi * freq1 * i / sample_rate)
                    wave2 = 1500 * math.sin(2 * math.pi * freq2 * i / sample_rate)
                    wave = int((wave1 + wave2) * (1 - i / frames))
                    arr.append([wave, wave])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "drop":
                # Piece drop sound
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 1000 * math.sin(2 * math.pi * 200 * i / sample_rate)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "rotate":
                # Rotation sound
                duration = 0.1
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    wave = 800 * math.sin(2 * math.pi * 400 * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
                
            elif sound_type == "game_over":
                # Game over sound
                duration = 1.0
                sample_rate = 22050
                frames = int(duration * sample_rate)
                arr = []
                for i in range(frames):
                    freq = 300 - (i * 200 / frames)
                    wave = 3000 * math.sin(2 * math.pi * freq * i / sample_rate) * (1 - i / frames)
                    arr.append([int(wave), int(wave)])
                sound = pygame.sndarray.make_sound(pygame.array.array('i', arr))
                sound.play()
        except:
            pass
    
    def load_high_score(self):
        """Load high score from file"""
        try:
            with open('tetris_high_score.json', 'r') as f:
                data = json.load(f)
                return data.get('high_score', 0)
        except:
            return 0
    
    def save_high_score(self):
        """Save high score to file"""
        try:
            with open('tetris_high_score.json', 'w') as f:
                json.dump({'high_score': self.high_score}, f)
        except:
            pass
    
    def spawn_piece(self):
        """Spawn a new piece"""
        if self.next_piece is None:
            self.current_piece = Piece(GRID_WIDTH // 2, 0, random.randint(0, 6))
            self.next_piece = Piece(GRID_WIDTH // 2, 0, random.randint(0, 6))
        else:
            self.current_piece = self.next_piece
            self.current_piece.x = GRID_WIDTH // 2
            self.current_piece.y = 0
            self.next_piece = Piece(GRID_WIDTH // 2, 0, random.randint(0, 6))
        
        self.can_hold = True
        
        # Check if game over
        if self.check_collision(self.current_piece):
            self.game_state = GameState.GAME_OVER
            self.play_sound("game_over")
            if self.score > self.high_score:
                self.high_score = self.score
                self.save_high_score()
    
    def check_collision(self, piece, rotation=None, x=None, y=None):
        """Check if piece collides with grid or boundaries"""
        cells = piece.get_cells(rotation, x, y)
        
        for cell_x, cell_y in cells:
            # Check boundaries
            if cell_x < 0 or cell_x >= GRID_WIDTH or cell_y >= GRID_HEIGHT:
                return True
            
            # Check collision with placed pieces (skip negative y for spawn)
            if cell_y >= 0 and self.grid[cell_y][cell_x]:
                return True
        
        return False
    
    def place_piece(self, piece):
        """Place piece on grid"""
        cells = piece.get_cells()
        
        for cell_x, cell_y in cells:
            if 0 <= cell_y < GRID_HEIGHT and 0 <= cell_x < GRID_WIDTH:
                self.grid[cell_y][cell_x] = piece.color
        
        self.play_sound("drop")
        self.create_particles(piece)
        self.clear_lines()
        self.spawn_piece()
    
    def create_particles(self, piece):
        """Create particle effects when piece is placed"""
        cells = piece.get_cells()
        for cell_x, cell_y in cells:
            if 0 <= cell_y < GRID_HEIGHT and 0 <= cell_x < GRID_WIDTH:
                screen_x = GRID_X_OFFSET + cell_x * CELL_SIZE + CELL_SIZE // 2
                screen_y = GRID_Y_OFFSET + cell_y * CELL_SIZE + CELL_SIZE // 2
                
                for _ in range(3):
                    velocity = (random.uniform(-3, 3), random.uniform(-5, -1))
                    self.particles.append(Particle(screen_x, screen_y, piece.color, velocity))
    
    def clear_lines(self):
        """Clear completed lines"""
        lines_to_clear = []
        
        # Find completed lines
        for y in range(GRID_HEIGHT):
            if all(self.grid[y]):
                lines_to_clear.append(y)
        
        if lines_to_clear:
            # Create particle effects for cleared lines
            for y in lines_to_clear:
                for x in range(GRID_WIDTH):
                    screen_x = GRID_X_OFFSET + x * CELL_SIZE + CELL_SIZE // 2
                    screen_y = GRID_Y_OFFSET + y * CELL_SIZE + CELL_SIZE // 2
                    
                    for _ in range(5):
                        velocity = (random.uniform(-5, 5), random.uniform(-8, -2))
                        self.particles.append(Particle(screen_x, screen_y, WHITE, velocity))
            
            # Remove cleared lines
            for y in reversed(lines_to_clear):
                del self.grid[y]
                self.grid.insert(0, [0 for _ in range(GRID_WIDTH)])
            
            # Update score and level
            lines_cleared = len(lines_to_clear)
            self.lines_cleared += lines_cleared
            
            # Scoring system
            line_scores = [0, 40, 100, 300, 1200]  # 0, 1, 2, 3, 4 lines
            if lines_cleared < len(line_scores):
                score_multiplier = self.level
                self.score += line_scores[lines_cleared] * score_multiplier
            
            # Level progression
            self.level = self.lines_cleared // 10 + 1
            self.fall_speed = max(50, 500 - (self.level - 1) * 50)
            
            # Play sound
            if lines_cleared == 4:
                self.play_sound("tetris")
            else:
                self.play_sound("line_clear")
    
    def move_piece(self, dx, dy):
        """Move current piece"""
        if self.current_piece:
            new_x = self.current_piece.x + dx
            new_y = self.current_piece.y + dy
            
            if not self.check_collision(self.current_piece, x=new_x, y=new_y):
                self.current_piece.x = new_x
                self.current_piece.y = new_y
                return True
        return False
    
    def rotate_piece(self):
        """Rotate current piece"""
        if self.current_piece:
            new_rotation = (self.current_piece.rotation + 1) % len(Tetromino.SHAPES[self.current_piece.shape])
            
            if not self.check_collision(self.current_piece, rotation=new_rotation):
                self.current_piece.rotation = new_rotation
                self.play_sound("rotate")
                return True
            
            # Try wall kicks
            kicks = [(0, 0), (-1, 0), (1, 0), (0, -1), (-1, -1), (1, -1)]
            for kick_x, kick_y in kicks:
                test_x = self.current_piece.x + kick_x
                test_y = self.current_piece.y + kick_y
                
                if not self.check_collision(self.current_piece, rotation=new_rotation, x=test_x, y=test_y):
                    self.current_piece.rotation = new_rotation
                    self.current_piece.x = test_x
                    self.current_piece.y = test_y
                    self.play_sound("rotate")
                    return True
        
        return False
    
    def hard_drop(self):
        """Drop piece to bottom instantly"""
        if self.current_piece:
            while self.move_piece(0, 1):
                self.score += 2  # Bonus points for hard drop
            self.place_piece(self.current_piece)
    
    def hold_piece(self):
        """Hold current piece"""
        if self.can_hold and self.current_piece:
            if self.held_piece is None:
                self.held_piece = Piece(0, 0, self.current_piece.shape)
                self.spawn_piece()
            else:
                # Swap current and held piece
                temp_shape = self.current_piece.shape
                self.current_piece.shape = self.held_piece.shape
                self.current_piece.rotation = 0
                self.current_piece.x = GRID_WIDTH // 2
                self.current_piece.y = 0
                
                self.held_piece.shape = temp_shape
                
                # Check if swapped piece can be placed
                if self.check_collision(self.current_piece):
                    # Revert if collision
                    self.current_piece.shape = self.held_piece.shape
                    self.held_piece.shape = temp_shape
                    return
            
            self.can_hold = False
    
    def get_ghost_piece(self):
        """Get ghost piece position (where piece would land)"""
        if not self.current_piece:
            return None
        
        ghost = deepcopy(self.current_piece)
        while not self.check_collision(ghost, y=ghost.y + 1):
            ghost.y += 1
        
        return ghost
    
    def update_particles(self):
        """Update particle effects"""
        self.particles = [p for p in self.particles if p.life > 0]
        for particle in self.particles:
            particle.update()
    
    def draw_grid(self):
        """Draw game grid"""
        # Grid background
        grid_rect = pygame.Rect(GRID_X_OFFSET, GRID_Y_OFFSET, 
                               GRID_WIDTH * CELL_SIZE, GRID_HEIGHT * CELL_SIZE)
        pygame.draw.rect(self.screen, BLACK, grid_rect)
        pygame.draw.rect(self.screen, WHITE, grid_rect, 2)
        
        # Draw placed pieces
        for y in range(GRID_HEIGHT):
            for x in range(GRID_WIDTH):
                if self.grid[y][x]:
                    rect = pygame.Rect(GRID_X_OFFSET + x * CELL_SIZE + 1,
                                     GRID_Y_OFFSET + y * CELL_SIZE + 1,
                                     CELL_SIZE - 2, CELL_SIZE - 2)
                    pygame.draw.rect(self.screen, self.grid[y][x], rect)
                    pygame.draw.rect(self.screen, WHITE, rect, 1)
        
        # Draw grid lines
        for x in range(GRID_WIDTH + 1):
            start_pos = (GRID_X_OFFSET + x * CELL_SIZE, GRID_Y_OFFSET)
            end_pos = (GRID_X_OFFSET + x * CELL_SIZE, GRID_Y_OFFSET + GRID_HEIGHT * CELL_SIZE)
            pygame.draw.line(self.screen, DARK_GRAY, start_pos, end_pos)
        
        for y in range(GRID_HEIGHT + 1):
            start_pos = (GRID_X_OFFSET, GRID_Y_OFFSET + y * CELL_SIZE)
            end_pos = (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE, GRID_Y_OFFSET + y * CELL_SIZE)
            pygame.draw.line(self.screen, DARK_GRAY, start_pos, end_pos)
    
    def draw_piece(self, piece, ghost=False):
        """Draw a tetris piece"""
        if not piece:
            return
        
        cells = piece.get_cells()
        color = piece.color if not ghost else (*piece.color, 100)
        
        for cell_x, cell_y in cells:
            if 0 <= cell_y < GRID_HEIGHT and 0 <= cell_x < GRID_WIDTH:
                rect = pygame.Rect(GRID_X_OFFSET + cell_x * CELL_SIZE + 1,
                                 GRID_Y_OFFSET + cell_y * CELL_SIZE + 1,
                                 CELL_SIZE - 2, CELL_SIZE - 2)
                
                if ghost:
                    # Draw ghost piece with transparency
                    ghost_surface = pygame.Surface((CELL_SIZE - 2, CELL_SIZE - 2), pygame.SRCALPHA)
                    ghost_surface.fill((*piece.color, 100))
                    self.screen.blit(ghost_surface, rect)
                    pygame.draw.rect(self.screen, piece.color, rect, 2)
                else:
                    pygame.draw.rect(self.screen, color, rect)
                    pygame.draw.rect(self.screen, WHITE, rect, 1)
    
    def draw_next_piece(self):
        """Draw next piece preview"""
        if not self.next_piece:
            return
        
        # Next piece box
        box_x = GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20
        box_y = GRID_Y_OFFSET + 50
        box_width = 120
        box_height = 80
        
        pygame.draw.rect(self.screen, BLACK, (box_x, box_y, box_width, box_height))
        pygame.draw.rect(self.screen, WHITE, (box_x, box_y, box_width, box_height), 2)
        
        # Label
        label = self.font_small.render("NEXT", True, WHITE)
        self.screen.blit(label, (box_x + 10, box_y - 25))
        
        # Draw piece
        shape = self.next_piece.get_rotated_shape(0)
        start_x = box_x + box_width // 2 - len(shape[0]) * 10
        start_y = box_y + box_height // 2 - len(shape) * 10
        
        for row, line in enumerate(shape):
            for col, cell in enumerate(line):
                if cell == '#':
                    rect = pygame.Rect(start_x + col * 20, start_y + row * 20, 18, 18)
                    pygame.draw.rect(self.screen, self.next_piece.color, rect)
                    pygame.draw.rect(self.screen, WHITE, rect, 1)
    
    def draw_held_piece(self):
        """Draw held piece"""
        # Held piece box
        box_x = GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20
        box_y = GRID_Y_OFFSET + 170
        box_width = 120
        box_height = 80
        
        pygame.draw.rect(self.screen, BLACK, (box_x, box_y, box_width, box_height))
        pygame.draw.rect(self.screen, WHITE, (box_x, box_y, box_width, box_height), 2)
        
        # Label
        color = WHITE if self.can_hold else GRAY
        label = self.font_small.render("HOLD", True, color)
        self.screen.blit(label, (box_x + 10, box_y - 25))
        
        # Draw held piece
        if self.held_piece:
            shape = self.held_piece.get_rotated_shape(0)
            start_x = box_x + box_width // 2 - len(shape[0]) * 10
            start_y = box_y + box_height // 2 - len(shape) * 10
            
            piece_color = self.held_piece.color if self.can_hold else GRAY
            
            for row, line in enumerate(shape):
                for col, cell in enumerate(line):
                    if cell == '#':
                        rect = pygame.Rect(start_x + col * 20, start_y + row * 20, 18, 18)
                        pygame.draw.rect(self.screen, piece_color, rect)
                        pygame.draw.rect(self.screen, WHITE, rect, 1)
    
    def draw_ui(self):
        """Draw game UI"""
        # Score
        score_text = self.font_medium.render(f"Score: {self.score}", True, WHITE)
        self.screen.blit(score_text, (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20, GRID_Y_OFFSET + 290))
        
        # Level
        level_text = self.font_medium.render(f"Level: {self.level}", True, WHITE)
        self.screen.blit(level_text, (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20, GRID_Y_OFFSET + 330))
        
        # Lines cleared
        lines_text = self.font_small.render(f"Lines: {self.lines_cleared}", True, WHITE)
        self.screen.blit(lines_text, (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20, GRID_Y_OFFSET + 370))
        
        # High score
        high_score_text = self.font_small.render(f"High: {self.high_score}", True, YELLOW)
        self.screen.blit(high_score_text, (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20, GRID_Y_OFFSET + 400))
        
        # Controls
        controls = [
            "Controls:",
            "←→ Move",
            "↓ Soft Drop",
            "↑ Rotate",
            "Space Hard Drop",
            "C Hold",
            "P Pause"
        ]
        
        for i, control in enumerate(controls):
            color = YELLOW if i == 0 else LIGHT_GRAY
            font = self.font_small if i == 0 else self.font_small
            text = font.render(control, True, color)
            self.screen.blit(text, (GRID_X_OFFSET + GRID_WIDTH * CELL_SIZE + 20, GRID_Y_OFFSET + 450 + i * 20))
    
    def draw_menu(self):
        """Draw main menu"""
        self.screen.fill(BLACK)
        
        # Title with animated effect
        title_y = WINDOW_HEIGHT // 2 - 150 + math.sin(pygame.time.get_ticks() * 0.003) * 10
        title_text = self.font_large.render("TETRIS", True, WHITE)
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, title_y))
        
        # Title glow effect
        for offset in range(3, 0, -1):
            glow_text = self.font_large.render("TETRIS", True, CYAN)
            glow_rect = glow_text.get_rect(center=(WINDOW_WIDTH // 2 + offset, title_y + offset))
            self.screen.blit(glow_text, glow_rect)
        
        self.screen.blit(title_text, title_rect)
        
        # Menu options
        menu_items = [
            "Press SPACE to Start",
            f"High Score: {self.high_score}",
            "",
            "Controls:",
            "Arrow Keys - Move/Rotate",
            "Space - Hard Drop",
            "C - Hold Piece",
            "P - Pause"
        ]
        
        for i, item in enumerate(menu_items):
            if item:
                color = YELLOW if "Press SPACE" in item else WHITE
                if "Controls:" in item:
                    color = CYAN
                elif i > 3 and item:
                    color = LIGHT_GRAY
                
                text = self.font_small.render(item, True, color)
                text_rect = text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50 + i * 30))
                self.screen.blit(text, text_rect)
    
    def draw_game_over(self):
        """Draw game over screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(200)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Game over text
        game_over_text = self.font_large.render("GAME OVER", True, RED)
        game_over_rect = game_over_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 100))
        self.screen.blit(game_over_text, game_over_rect)
        
        # Final score
        final_score_text = self.font_medium.render(f"Final Score: {self.score}", True, WHITE)
        final_score_rect = final_score_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50))
        self.screen.blit(final_score_text, final_score_rect)
        
        # High score
        if self.score == self.high_score and self.score > 0:
            new_record_text = self.font_small.render("NEW HIGH SCORE!", True, YELLOW)
            new_record_rect = new_record_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 20))
            self.screen.blit(new_record_text, new_record_rect)
        
        # Lines cleared
        lines_text = self.font_small.render(f"Lines Cleared: {self.lines_cleared}", True, WHITE)
        lines_rect = lines_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20))
        self.screen.blit(lines_text, lines_rect)
        
        # Instructions
        restart_text = self.font_small.render("Press R to restart or ESC for menu", True, LIGHT_GRAY)
        restart_rect = restart_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 80))
        self.screen.blit(restart_text, restart_rect)
    
    def draw_pause(self):
        """Draw pause screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        overlay.set_alpha(150)
        overlay.fill(BLACK)
        self.screen.blit(overlay, (0, 0))
        
        # Pause text
        pause_text = self.font_large.render("PAUSED", True, YELLOW)
        pause_rect = pause_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
        self.screen.blit(pause_text, pause_rect)
        
        # Instructions
        continue_text = self.font_small.render("Press P to continue", True, WHITE)
        continue_rect = continue_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 50))
        self.screen.blit(continue_text, continue_rect)
    
    def reset_game(self):
        """Reset game to initial state"""
        self.grid = [[0 for _ in range(GRID_WIDTH)] for _ in range(GRID_HEIGHT)]
        self.current_piece = None
        self.next_piece = None
        self.held_piece = None
        self.can_hold = True
        self.score = 0
        self.level = 1
        self.lines_cleared = 0
        self.fall_speed = 500
        self.particles = []
        self.spawn_piece()
        self.game_state = GameState.PLAYING
    
    def handle_events(self):
        """Handle game events"""
        current_time = pygame.time.get_ticks()
        
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    if self.game_state == GameState.PLAYING:
                        self.game_state = GameState.MENU
                    elif self.game_state in [GameState.GAME_OVER, GameState.PAUSED]:
                        self.game_state = GameState.MENU
                    elif self.game_state == GameState.MENU:
                        return False
                
                elif event.key == pygame.K_SPACE:
                    if self.game_state == GameState.MENU:
                        self.reset_game()
                    elif self.game_state == GameState.PLAYING:
                        self.hard_drop()
                
                elif event.key == pygame.K_r and self.game_state == GameState.GAME_OVER:
                    self.reset_game()
                
                elif event.key == pygame.K_p:
                    if self.game_state == GameState.PLAYING:
                        self.game_state = GameState.PAUSED
                    elif self.game_state == GameState.PAUSED:
                        self.game_state = GameState.PLAYING
                
                elif self.game_state == GameState.PLAYING:
                    if event.key == pygame.K_LEFT:
                        self.move_piece(-1, 0)
                        self.key_repeat['left'] = current_time + 250  # Initial delay
                    elif event.key == pygame.K_RIGHT:
                        self.move_piece(1, 0)
                        self.key_repeat['right'] = current_time + 250
                    elif event.key == pygame.K_DOWN:
                        if self.move_piece(0, 1):
                            self.score += 1  # Bonus for soft drop
                        self.key_repeat['down'] = current_time + 100
                    elif event.key == pygame.K_UP:
                        self.rotate_piece()
                    elif event.key == pygame.K_c:
                        self.hold_piece()
            
            elif event.type == pygame.KEYUP:
                if event.key == pygame.K_LEFT:
                    self.key_repeat['left'] = 0
                elif event.key == pygame.K_RIGHT:
                    self.key_repeat['right'] = 0
                elif event.key == pygame.K_DOWN:
                    self.key_repeat['down'] = 0
        
        # Handle key repeat for smooth movement
        keys = pygame.key.get_pressed()
        if self.game_state == GameState.PLAYING:
            if keys[pygame.K_LEFT] and self.key_repeat['left'] > 0 and current_time > self.key_repeat['left']:
                self.move_piece(-1, 0)
                self.key_repeat['left'] = current_time + 50  # Repeat rate
            
            if keys[pygame.K_RIGHT] and self.key_repeat['right'] > 0 and current_time > self.key_repeat['right']:
                self.move_piece(1, 0)
                self.key_repeat['right'] = current_time + 50
            
            if keys[pygame.K_DOWN] and self.key_repeat['down'] > 0 and current_time > self.key_repeat['down']:
                if self.move_piece(0, 1):
                    self.score += 1
                self.key_repeat['down'] = current_time + 50
        
        return True
    
    def update(self):
        """Update game state"""
        if self.game_state == GameState.PLAYING:
            current_time = pygame.time.get_ticks()
            
            # Auto fall
            if current_time - self.last_fall > self.fall_speed:
                if not self.move_piece(0, 1):
                    self.place_piece(self.current_piece)
                self.last_fall = current_time
        
        # Always update particles
        self.update_particles()
    
    def draw(self):
        """Draw everything"""
        self.screen.fill(BLACK)
        
        if self.game_state == GameState.MENU:
            self.draw_menu()
        else:
            # Draw game elements
            self.draw_grid()
            
            # Draw ghost piece
            ghost = self.get_ghost_piece()
            if ghost:
                self.draw_piece(ghost, ghost=True)
            
            # Draw current piece
            if self.current_piece:
                self.draw_piece(self.current_piece)
            
            # Draw UI
            self.draw_next_piece()
            self.draw_held_piece()
            self.draw_ui()
            
            # Draw overlays
            if self.game_state == GameState.PAUSED:
                self.draw_pause()
            elif self.game_state == GameState.GAME_OVER:
                self.draw_game_over()
        
        # Draw particles
        for particle in self.particles:
            particle.draw(self.screen)
        
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
    print("🧩 Tetris - Ultimate Edition")
    print("=" * 30)
    print("Features:")
    print("• Classic Tetris gameplay with all 7 tetrominoes")
    print("• Ghost piece preview showing drop position") 
    print("• Hold piece functionality")
    print("• Next piece preview")
    print("• Line clearing with particle effects")
    print("• Progressive level system")
    print("• Wall kicks for smooth rotation")
    print("• High score tracking (saved to file)")
    print("• Sound effects for all actions")
    print("• Smooth controls with key repeat")
    print("\nControls:")
    print("• Arrow Keys - Move and rotate pieces")
    print("• Space - Hard drop (instant drop)")
    print("• C - Hold current piece")
    print("• P - Pause/Resume game")
    print("• R - Restart (when game over)")
    print("• ESC - Menu/Quit")
    print("\nScoring:")
    print("• 1 line: 40 × level")
    print("• 2 lines: 100 × level")
    print("• 3 lines: 300 × level")
    print("• 4 lines (Tetris): 1200 × level")
    print("• Soft drop: +1 point per cell")
    print("• Hard drop: +2 points per cell")
    print("\nStarting game...")
    
    try:
        game = TetrisGame()
        game.run()
    except KeyboardInterrupt:
        print("\nGame interrupted by user")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
